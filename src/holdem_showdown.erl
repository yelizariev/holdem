%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2013 Ivan Yelizariev <ivann.exe@gmail.com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

-module(holdem_showdown).
-include("holdem.hrl").
-include("holdem_internal.hrl").

-export([do/1,
         do_single_player/1,
         next/1,
         boast/2]).
-export([clear_losers/1,
         clear_winner_chips/1,
         clear_winner_cards/1,
         onclear/1]).
-spec do(sd())->
		        #done{}.
%% @doc перед вызовом все игроки с картами в #info.banks и
%% #game.done
do({I = #info{banks=Banks },
    #game{board_cards=Cards,
          done=NotAllin}})->
	[LastBank|Tail] =
		case Banks  of
			[#bank{chips=Chips}, LB | T]
			  when Chips=:=0 ->
				[LB|T];
			_->
				Banks
		end,
	LastBankPlayers=NotAllin++LastBank#bank.players,
	#p{seat=FirstOpen}=lists:last(LastBankPlayers),
	BanksNoForce = [LastBank#bank{players=LastBankPlayers} |Tail],
	NewBanks =
		[B#bank{players=
		        [P#p{force=
		             holdem_cards:force([P#p.card1, P#p.card2
		                                 | Cards])
		             %% OLD: is_open=false
		            } || P <- B#bank.players]
		       }
		 || B <- BanksNoForce],
	FGame = #fgame{board_cards=Cards},
	if
		length(LastBankPlayers)=:=1->
			%% выдаем сдачу игроку, чью ставку полностью никто не уровнял
			[B1, B2 | BB] = NewBanks,
			[P]=B1#bank.players,
			BC=[#bank_for_winner{seats=[P#p.seat], chips=B1#bank.chips}],
			NewB2=B2#bank{players=[P#p{bet=P#p.bet+B1#bank.chips}|B2#bank.players]},
			SD=prepare_next_bank({I#info{banks=[NewB2|BB]}, FGame}, P#p.seat),
			Timers=[{?TIMER(I, before_showdown_next_bank_after_single_player),
			         showdown_next}],
			#done{sd=SD, timers=Timers, broadcast=BC};
		true->
			next(prepare_next_bank({I#info{banks=NewBanks}, FGame}, FirstOpen))
	end.
	%% OLD;
	%{Players, Out}=showdown(Banks, FirstOpen, [], []),
	%
	%NewPlayers=Players++I#info.players,
	%Stacks=[{Seat, Chips} || #p{seat=Seat, stack=Chips} <- NewPlayers],
	%
	%{{I#info{players=NewPlayers}, nogame},
	% lists:reverse([#end_game{rake=G#game.rake_chips, stacks=Stacks} | Out])}.
do_single_player({I = #info{banks=Banks},
                  #game{board_cards=Cards,
                        opened_board_cards=OpenedCards,
                        done=NotAllin}})->
	%?INFO(I),
	B=#bank{players=Allin}=
		case Banks of
			[_EmptyBank, B_]->B_;
			[B_]->B_
		end,
	[P]=
		case {Allin, NotAllin} of
			{[], [_]}-> NotAllin;
			{[_], []} -> Allin
		end,
	NewCards =
		case {OpenedCards, Cards} of
			{preflop, _}->
				[];
			{flop, [F1,F2,F3,_T,_R]}->
				[F1,F2,F3];
			{turn, [F1,F2,F3,T,_R]}->
				[F1,F2,F3,T];
			{river, [F1,F2,F3,T,R]}->
				[F1,F2,F3,T,R]
		end,
	SD = {I#info{banks=[],
	             allin=[]},
	      #fgame{board_cards=NewCards,
	             is_single_player=true,
	             chips=B#bank.chips,
	             winners=[P]}},
	#done{sd=SD,
	      broadcast = [{P#p.seat, #req_boast{}}],
	      timers = ?TIMER(I, single_player_clear_table) ++ [{0, showdown_onclear}]
	     }.


-spec next(sd())->
		          #done{}.
next({I, G})->
	PLen = length(G#fgame.players),
	if
		PLen>0 ->
			next_player({I,G});
		true->
			next_bank({I,G})
	end.

-spec boast(sd(), seat())->
		           #done{}.
boast({I,G = #fgame{used_cards=OldUsedCards,
                    winners=Winners,
                    losers=Losers}}, Seat)->
	case {lists:keytake(Seat, #p.seat, Losers), Winners} of
		{_, [P = #p{seat=PSeat, card1=C1, card2=C2, is_open=IsOpen}]}
		  when PSeat=:=Seat, not IsOpen, C1/=false->
			Broadcast= [#open_player_cards{seat=Seat, card1=C1, card2=C2}],
			#done{sd={I, G#fgame{winners=[P#p{is_open=true}]}},
			      broadcast=Broadcast};
		{{value, P, LosersTail}, _}
		  when not P#p.is_open->
			[#p{force=BestForce}|_]=Winners,
			case {P#p.force, BestForce} of
				{#force{type=T1}, #force{type=T2}}
				  when T1<T2->
					{NewP, BC, _CurHighlight}=show_comb(P, 0, weaker_boast),
					#done{sd={I,G#fgame{losers=[NewP|LosersTail]}},
					      broadcast=BC};
				{#force{type=T1, ecards=Cards1}, #force{type=T2, ecards=Cards2}}
				  when T1=:=T2->
					{second, UsedCards} = holdem_cards:best_ranks(Cards1, Cards2),
					{NewP, BC, _CurHighlight}=show_comb(P, UsedCards, weaker_boast),
					{NewWinners, NewBC} =
						update_comb(G#fgame.winners, UsedCards, OldUsedCards, BC, all),
					#done{sd={I,G#fgame{losers=[NewP|LosersTail],
					                    winners=NewWinners,
					                    used_cards=UsedCards}},
					      broadcast=NewBC}
			end;
		_ ->
			#done{sd={I,G}}
	end.

%%% ===========================================
%%%   clear
%%% -------------------------------------------
clear_losers({I,G = #fgame{losers=Losers}})->
	{NewLosers, Stacks, Seats} = clear_losers(Losers, [], [], []),
	BC = [#clear_player_cards{seats=Seats}|
	      case Stacks of
		      []-> [];
		      _-> [#clear_winner_chips{stacks=Stacks}]
	      end],
	SD = {I#info{players=NewLosers++I#info.players},
	      G#fgame{losers=[]}},
	NewSD = clear_cur_highlight(Seats, SD),
	#done{sd=NewSD, broadcast=BC}.

clear_losers([P|Players], Stacks, PAcc, Seats)->
	NewP = P#p{stack = P#p.stack+P#p.bet,
	           bet=0,
	           card1 = false,
	           card2 = false},
	NewStacks =
		if
			P#p.bet=:=0->
				Stacks;
			true ->
				[{P#p.seat, NewP#p.stack}|Stacks]
		end,
	clear_losers(Players, NewStacks, [NewP|PAcc], [P#p.seat|Seats]);
clear_losers([], Stacks, PAcc, Seats) ->
	{PAcc, Stacks, Seats}.

clear_winner_chips({I,G})->
	{NewWinners, Stacks} = clear_winner_chips(G#fgame.winners, [], []),
	#done{sd={I,G#fgame{winners=NewWinners}},
	      broadcast=[#clear_winner_chips{stacks=Stacks}]}.

clear_winner_chips([P|Players], Stacks, PAcc)->
	NewP = P#p{stack = P#p.stack+P#p.bet,
	           bet=0},
	clear_winner_chips(Players,
	                   [{P#p.seat, NewP#p.stack}|Stacks],
	                   [NewP|PAcc]);
clear_winner_chips([], Stacks, PAcc) ->
	{PAcc, Stacks}.

clear_winner_cards({I,G = #fgame{winners=Winners}})->
	Seats = [S || #p{seat=S} <- Winners],
	NewWinners = [P#p{card1=false, card2=false} || P <- Winners],
	BC = [#clear_player_cards{seats=Seats}],
	SD = {I,G#fgame{winners=NewWinners}},
	NewSD = clear_cur_highlight(Seats, SD),
	#done{sd=NewSD, broadcast=BC}.

onclear({I, G})->
	Autostart = I#info.autostart,
	#done{sd={I#info{players=G#fgame.winners++I#info.players},
	          #nogame{
	            timer=case Autostart of
		                  true->button;
		                  false->false
	                  end,
	            sb_seat_old=I#info.sb_seat,
	            bb_seat_old=I#info.bb_seat,
	            button_old =I#info.button
	           }},
	      timers=
		      [
		       {0, on_switch_to_nogame}
		       |
		       case Autostart of
			       true->[{?TIMER(I,before_button), button}];
			       false -> []
		       end
		      ]
	     }.

%%% ===========================================
%%%   next_player
%%% -------------------------------------------
-spec next_player(sd())->
		                 #done{}.
next_player({I,
             G = #fgame{players=[P|Players],
                        winners=Winners}})
  when Winners=:=[]->
	{NewP, BC, CurHighlight} = show_comb(P, 0, stronger),
	#done{sd = {I,G#fgame{players=Players, winners=[NewP], cur_highlight=CurHighlight}},
	      broadcast=BC,
	      timers=[{?TIMER(I, after_player_showdown), showdown_next}]
	     };
next_player({I, G = #fgame{players=[P|Players]}})->
	{NewG, BC, IsWeaker} = player_vs_winners(P, G#fgame{players=Players}),
	Timers =
		if
			Players=:=[]->
				[{?TIMER(I, before_move_bank_to_winner), showdown_next}];
			IsWeaker->
				[{?TIMER(I, after_player_weaker), showdown_next}];
			true ->
				[{?TIMER(I, after_player_showdown), showdown_next}]
		end,
	#done{sd={I,NewG},
	      broadcast=BC,
	      timers=Timers}.

%% @doc boolean() -- IsWeaker
-spec player_vs_winners(#p{}, #fgame{})->
		                       {#fgame{}, broadcast(), boolean()}.
player_vs_winners(P, G = #fgame{winners=Winners,
                                losers=Losers})->
	[#p{force=BestForce}|_]=Winners,
	case {P#p.force, BestForce} of
		{#force{type=T1}, #force{type=T2}}
		  when T1>T2->
			{NewP, BC, CurHighlight} = show_comb(P, 0, stronger),
			{G#fgame{winners=[NewP],
			         cur_highlight=CurHighlight,
			         losers=Winners++Losers},
			 BC, false};
		{#force{type=T1}, #force{type=T2}}
		  when T1<T2->
			{NewP, BC, CurHighlight_} = show_comb(P, 0, weaker),
			CurHighlight=
				if
					CurHighlight_=:=false->
						G#fgame.cur_highlight;
					true->
						CurHighlight_
				end,
			{G#fgame{losers=[NewP|Losers],
			         cur_highlight=CurHighlight},
			 BC, true};
		{#force{ecards=Cards1}, #force{ecards=Cards2}} ->
			player_vs_winners(P, G, holdem_cards:best_ranks(Cards1, Cards2))
	end.


player_vs_winners(P, G = #fgame{winners=Winners, losers=Losers, used_cards=OldUsedCards},
                        {first, UsedCards})->
	{NewP, BC, CurHighlight}=show_comb(P, UsedCards, stronger),
	{Winners1, NewBC} =
		update_comb(Winners, UsedCards, OldUsedCards, BC, all),
	{G#fgame{winners=[NewP],
	         losers=Winners1++Losers,
	         cur_highlight=CurHighlight,
	         used_cards=UsedCards},
	 NewBC, false};
player_vs_winners(P, G = #fgame{winners=Winners, losers=Losers, used_cards=OldUsedCards},
                        {second, UsedCards})
  when P#p.is_open->
	{NewP, BC, CurHighlight}=show_comb(P, UsedCards, weaker),
	{NewWinners, NewBC} =
		update_comb(Winners, UsedCards, OldUsedCards, BC, all),
	{G#fgame{winners=NewWinners,
	         losers=[NewP|Losers],
	         cur_highlight=CurHighlight,
	         used_cards=UsedCards},
	 NewBC, true};
player_vs_winners(P, G = #fgame{winners=Winners, losers=Losers, used_cards=OldUsedCards},
                        {second, UsedCards}) ->
	%% карты игрока не открыты
	{NewP, BC, _CurHighlight}=show_comb(P, UsedCards, weaker),
	{_Winners, NewBC} = update_comb(Winners, UsedCards, OldUsedCards, BC, P#p.seat),
	{G#fgame{losers=[NewP|Losers]},
	 NewBC,
	 true};
player_vs_winners(P, G = #fgame{winners=Winners, used_cards=OldUsedCards},
                        {equal, UsedCards})->
	{NewP, BC, CurHighlight}=show_comb(P, UsedCards, stronger),
	{Winners1, NewBC} =
		update_comb(Winners, UsedCards, OldUsedCards, BC, all),
	{G#fgame{winners=[NewP|Winners1],
	         cur_highlight=CurHighlight,
	         used_cards=UsedCards},
	 NewBC, false}.


%%% ===========================================
%%%   next_player -- вспомогательные функции
%%% -------------------------------------------

update_comb(Players, UsedCards, OldUsedCards, BCAcc, _To)
  when UsedCards=:=OldUsedCards->
	{Players, BCAcc};
update_comb(Players, UsedCards, _OldUsedCards, BCAcc, To)->
	{NewPlayers, Value}=update_comb(Players, UsedCards, [], []),
	{NewPlayers, [{To, #showdown_update_comb{seat=Seat,comb=Comb}} || {Seat, Comb} <- Value]++BCAcc}.

update_comb([P|Players], UsedCards, PAcc, ValueAcc)->
	Comb = holdem_cards:force_to_comb(P#p.force, UsedCards, false),
	NewP = P#p{comb=Comb},
	update_comb(Players, UsedCards, [NewP|PAcc],
	            [{P#p.seat, Comb}|ValueAcc]);
update_comb([], _, PAcc, ValueAcc) ->
	{PAcc, ValueAcc}.


-spec show_comb(#p{}, integer(), weaker|weaker_boast|stronger)->
		               {#p{}, broadcast(), false|{seat(), highlight()}}.
show_comb(P = #p{is_open=IsOpen,
                 card1=C1,
                 card2=C2,
                 force=Force,
                 seat=Seat}, UsedCards, WorS)->
	Comb=holdem_cards:force_to_comb(Force, UsedCards, WorS=/=weaker_boast),
	BC =
		case {WorS, IsOpen} of
			{stronger, true}->
				[#showdown_stronger_allin{seat=Seat, comb=Comb}];
			{stronger, false}->
				[#showdown_stronger{seat=Seat, comb=Comb, card1=C1, card2=C2}];
			{weaker, true}->
				[#showdown_weaker{seat=Seat, comb=Comb}];
			{weaker, false}->
				[{Seat, #showdown_weaker{seat=Seat, comb=Comb}},
				 {Seat, #req_boast{}},
				 {{except, Seat}, #showdown_weaker{seat=Seat}}];
			{weaker_boast, false}->
				[#showdown_weaker_boast{seat=Seat, comb=Comb, card1=C1, card2=C2}]
		end,
	NewP=
		case {WorS, IsOpen} of
			{weaker, false}->
				P#p{last_action=weaker};
			{Weaker, _} when Weaker=/=stronger->
				P#p{last_action=weaker_comb, comb=Comb};
			_->
				P#p{last_action=comb, is_open=true, comb=Comb}
		end,
	CurHighlight=
		case {WorS, IsOpen} of
			{weaker_boast, _}->false;
			{weaker, false}->false;
			_->{Seat, Comb#comb.highlight}
		end,
	{NewP, BC, CurHighlight}.

clear_cur_highlight([S|_Seats], {I,G = #fgame{cur_highlight={CurSeat, _Highlight}}})
  when CurSeat=:=S->
	{I,G#fgame{cur_highlight=false}};
clear_cur_highlight([_S|Seats], {I,G = #fgame{cur_highlight={_CurSeat, _Highlight}}})->
	clear_cur_highlight(Seats, {I,G});
clear_cur_highlight(_Seats, {I,G})->
	{I,G}.

-spec next_bank(sd())->
		               #done{}.
%%% ===========================================
%%%   next_bank
%%% -------------------------------------------
next_bank({I = #info{banks=Banks},
           G = #fgame{winners=Winners,
                      losers=Losers,
                      chips=Chips}})->
	%% move bank to winner
	{BC, NewWinners}=
		case Winners of
			[]->
				{[#bank_for_winner{seats=[],
				                  chips=Chips}],
				 []};
			[P = #p{seat=WSeat}]->
				{[#bank_for_winner{seats=[WSeat],
				                   chips=Chips}],
				 [P#p{bet=P#p.bet+Chips}]};
			_->
				ChipsForOne = Chips div length(Winners),
				{[#bank_for_winner{seats=[S || #p{seat=S} <- Winners],
				                   chips=ChipsForOne}],
				 [W#p{bet=W#p.bet+ChipsForOne} || W <- Winners]}
		end,
	SD = {I, G#fgame{winners=NewWinners}},
	BLen = length(Banks),
	case Winners of
		[]->
			%% случай когда победитель вышел из игры (unseat) не
			%% дождавшись получения выигрыша
			#done{sd=prepare_next_bank(SD, 0),
			      timers = if
				               BLen>0 -> [{0, showdown_next}];
				               true-> ?TIMER(I,clear_table)++[{0, showdown_onclear}]
			               end,
			      broadcast = BC
			     };
		[#p{seat=FirstOpen}|_]
		  when BLen>0 ->
			%% есть еще банки
			ShowdownNext={?TIMER(I, before_showdown_next_bank), showdown_next},
			#done{sd=prepare_next_bank(SD, FirstOpen),
			      broadcast=BC,
			      timers=if
				             Losers=:=[]->
					             [ShowdownNext];
				             true->
					             [{?TIMER(I, before_bank_losers), showdown_clear_losers},
					              ShowdownNext]
			             end
			     };
		_ when G#fgame.is_single_player->
			%% see do_single_player
			#done{sd=SD,
			      broadcast=BC};
		_ ->
			%% больше банков нет
			#done{sd=SD,
			      timers= ?TIMER(I,clear_table)++[{0, showdown_onclear}],
			      broadcast=BC
			     }
	end.

-spec prepare_next_bank(sd(), seat())->sd().
prepare_next_bank({I = #info{banks=[B|Banks]}, G}, FirstOpen)->
	Players=holdem_sort:center(B#bank.players, FirstOpen),
	{I#info{banks=Banks}, G#fgame{players=Players,
	                              used_cards=0,
	                              chips=B#bank.chips}}.

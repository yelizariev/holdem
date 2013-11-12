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

-module(holdem_next).
-include("holdem.hrl").
-include("holdem_internal.hrl").
-export([preflop/1,
         premature_showdown/1,
         new_round_next_move/1,
         next_player_move/1,
         holdem_in/2]).
%%% ===========================================
%%%   Продолжение игры
%%% -------------------------------------------

%% @doc обработать сообщение от игрока
-spec holdem_in(sd(), holdem_in())->
		               #done{}.
holdem_in(SD, agree_blind)->
	{ok, PayBlind, NewSD}= holdem_blind:pay(SD),
	Done = holdem_pregame:next_blind(NewSD),
	?EXTEND_BROADCAST(PayBlind, Done);
holdem_in(SD, disagree_blind) ->
	{ok, NotPayBlind, NewSD} = holdem_blind:not_pay(SD),
	Done = holdem_pregame:next_blind(NewSD),
	?EXTEND_BROADCAST(NotPayBlind, Done);
holdem_in({I,
           G = #game{cur_bet=CurBet,
                     min_raise=MinRaise,
                     max_raise=MaxRaise,
                     done=PlayersDone,
                     wait=[P= #p{bet=PBet, seat=Seat, stack=Stack} |
                           Wait]}},
          {move, MoveType0})->
	MoveType=
		if
			is_integer(MoveType0), MaxRaise==0->
				call;
			is_integer(MoveType0), (MoveType0 > MaxRaise)->
				MaxRaise;
			is_integer(MoveType0), (MoveType0 < MinRaise)->
				call;
			true ->
				MoveType0
		end,
	{SD, ShowMoveType, BetData} =
		if
			MoveType=:=autofold;
			MoveType=:=fold;
			MoveType=:=autocheck, CurBet > PBet
			-> %% Fold
				PlayersOut =
					case PBet>0 of
						false->
							I#info.dead;
						true->
							[#bet{seat=Seat, bet=PBet, batches=P#p.batches}
							 |I#info.dead]
					end,
				NewP=P#p{last_action=fold, bet=0, batches=[0]},
				{{I#info{players=[NewP|I#info.players],
				         dead=PlayersOut},
				  G#game{wait=Wait}},
				  case MoveType of
					  fold-> fold;
					  _-> autofold
				  end,
				 #bet_data{seat=Seat}};
			MoveType=:=autocheck;
			MoveType=:=call, CurBet=:=PBet
			-> %% Check
				NewP=P#p{last_action=check},
				{{I, G#game{done=[NewP|PlayersDone],
				            wait=Wait}},
				 check,
				#bet_data{seat=Seat}};
			MoveType=:=call,
			CurBet>Stack+PBet
			-> %% Call allin
				player_adding_chips({I, G}, Stack+PBet, call);
			MoveType=:=call
			-> %% Call
				player_adding_chips({I, G}, CurBet, call);
			true
			-> %% Raise
				player_adding_chips({I, G}, MoveType, raise)
		end,
	Done = after_move(SD),
	?EXTEND_BROADCAST(#show_player_move{type=ShowMoveType, data=BetData}, Done);
holdem_in(SD, _) ->
	#done{sd=SD}.

-spec preflop(sd())->
		                #done{}.
preflop({I, G = #game{wait=Wait}})->
	after_move({I, G#game{wait=new_game_prepare_players(Wait)}}).


%%% ===========================================
%%%   Внутренние функции
%%% -------------------------------------------
new_round_prepare_players(Players)->
	[P#p{is_open=false, last_action=false, premove=false} || P<-Players].
new_game_prepare_players(Players)->
	[P#p{is_open=false,
	     last_action=false,
	     premove=false,
	     comb=false}
	 || P<-Players].

-spec after_move(sd())->
		                #done{}.
after_move({I, G = #game{cur_bet=CurBet,
                         wait=Wait,
                         done=Done}})->
	Len = length(Done),
	case Wait of
		[P] when P#p.bet=:=CurBet,
		         Len=:=0->
			%% игрок на ББ и передним только пас и аллин меньше блайнда
			premature_showdown({I, G#game{wait=[], done=[P]}});
		[] when Len<2->
			premature_showdown({I,G});
		[]->
			next_round({I,G});
		_->
			next_player_move({I,G})
	end.





%%% ===========================================
%%%   you_move, now_move, premove
%%% -------------------------------------------
-spec next_player_move(sd())->
		                      #done{}.
next_player_move({I, G = #game{wait=[P|_]}})->
	AutoPlayer = lists:member(P#p.seat, I#info.autoplayers),
	if
		AutoPlayer->
			holdem_in({I,G}, {move, autocheck});
		true->
			do_next_player_move({I,G})
	end.
do_next_player_move({I, G=#game{cur_bet=CurBet,
                             wait=[P|Wait]}})->
	{ok, YouMove, MinRaise, MaxRaise} = holdem_move:you({I, G}, P),
	{TimerType, TimerDelay}=
		case ?TIMER(I, move_prewarning) of
			0-> {warning, ?TIMER(I, move_warning)};
			Delay -> {prewarning, Delay}
		end,
	Timer =
		if
			TimerType==warning->
				TimerDelay;
			true->
				false
		end,
	NowMove=#now_move{seat=P#p.seat,
	                  mseconds = Timer},
	{ok, PreMoveList, NewWait}=holdem_move:pre(Wait, CurBet),
	#done{sd={I, G#game{wait=[P#p{premove=CurBet} | NewWait],
	                    cur_you_move=YouMove,
	                    min_raise=MinRaise,
	                    max_raise=MaxRaise}},
	      cur = {P#p.seat, move},
	      cur_timer={TimerType, TimerDelay},
	      broadcast = [YouMove, NowMove | PreMoveList]
	     }.

%%% ===========================================
%%%   next(_, {move, _}) functions
%%% -------------------------------------------
-spec player_adding_chips(sd(), NewBet::chips(), Action::call|raise) ->
		      {sd(), call|raise, #bet_data{}}.
player_adding_chips({I = #info{allin=AllinPlayers,
                               banks=Banks},
                     G = #game{cur_bet=CurBet,
                               delta_raise=DeltaRaise,
                               wait=[P|Wait],
                               done=Done
                              }
                    },
                    NewBet,
                    Action)->
	{DeltaBatches, NewBatches}=
		holdem_batches:bet(AllinPlayers, NewBet, P#p.batches, length(Banks)-1),
	NewP=P#p{stack=P#p.stack-NewBet+P#p.bet,
	         bet=NewBet,
	         last_action=Action,
	         batches=NewBatches},
	NewG=
		case Action of
			call->
				 G#game{wait=Wait};
			raise->
				G#game{delta_raise=erlang:max(DeltaRaise, NewBet-CurBet),
				       cur_bet=NewBet,
				       wait=Wait++lists:reverse(Done),
				       %%wait=Wait++[PP#p{is_acted=true}
				       %%            || PP <- lists:reverse(Done)],
				       done=[]}
		end,
	{Allin, SD}=
		case NewP#p.stack=:=0 of
			false->
				{false, {I, NewG#game{
				              done=[NewP|NewG#game.done]
				             }}};
			true->
				{length(Banks)+length(NewBatches)-2,
				 {I#info{allin=[NewP|I#info.allin]}, NewG}}
		end,
	{SD, Action, #bet_data{seat=P#p.seat,
	                        batches=DeltaBatches,
	                        allin=Allin}}.


%%% ===========================================
%%%   next_round
%%% -------------------------------------------
-spec next_round(sd())->
		                #done{}.
next_round({I,G = #game{opened_board_cards=Level}})->
	{SD, BCFixBatches, CountBanks} =
		holdem_batches:fix({I,G}),
	Timers =
		if
			Level=:=river,
			CountBanks=:=1->
				?TIMER(I, after_river) ++ [{?TIMER(I, before_showdown), start_showdown}];
			CountBanks=:=1->
				?TIMER(I, after_round)++ [{?TIMER(I, before_new_round), new_round_next_move}];
			true->
				[{?TIMER(I, multipot_before_collecting_bets), collect_bets},
				 {CountBanks*?TIMER(I, multipot_after_collecting_bets), clear_moves}
				 | case Level of
					   river-> [{0, start_showdown}];
					   _-> [{0, open_board}, {0, new_round_next_move}]
				   end
				]
		end,
	#done{sd=SD,
	      cur=nobody,
	      broadcast=BCFixBatches,
	      timers=Timers}.

-spec new_round_next_move(sd())-> #done{}.
new_round_next_move({I,G})->
	%% [Button++, Button++2, ..., Button--, Button]
	PlayersWait=holdem_sort:right(G#game.done, I#info.button),
	SD = {I, G#game{wait=new_round_prepare_players(PlayersWait),
	                cur_bet=0,
	                delta_raise=I#info.bb,
	                done=[]}},
	next_player_move(SD).

%%% ===========================================
%%%   досрочный showdown
%%% -------------------------------------------
-spec premature_showdown(sd())->
		                        #done{}.
%% @doc все игроки с картами в #game.done, #info.allin или #game.banks
premature_showdown({I,G})->
	{SD, BCFixBatches, CountBanks} =
		holdem_batches:fix({I,G}),
	SinglePlayer=
		case {I#info.allin, G#game.done} of
			_ when length(I#info.banks)>1->
				false;
			{[], [P]}->
				P;
			{[P], []} ->
				P;
			_ ->
				false
		end,
	Level = G#game.opened_board_cards,
	Timers =
		if
			SinglePlayer=/=false->
				[
				 {?TIMER(I, single_player_before_collect_bets       ), collect_bets},
				 {0, start_showdown_single_player}
				];
			Level=:=river,
			CountBanks=:=1->
				?TIMER(I, after_river) ++ [{?TIMER(I, before_showdown), start_showdown}];
			true->
				TBefore = ?TIMER(I, multipot_before_collecting_bets),
				TAfter = CountBanks*?TIMER(I, multipot_after_collecting_bets),
				TFirst = ?TIMER(I, before_opening_board_allin_first),
				TNext = ?TIMER(I, before_opening_board_allin),
				TEnd = ?TIMER(I, before_showdown_allin),
				[{TBefore, collect_bets}
				 | case Level of
					   preflop ->
						   [{TAfter, open_player_cards},
						    {TFirst, open_board},
						    {TNext, open_board},
						    {TNext, open_board},
						    {TEnd, start_showdown}];
					   flop->
						   [{TAfter, open_player_cards},
						    {TFirst, open_board},
						    {TNext, open_board},
						    {TEnd, start_showdown}];
					   turn->
						   [{TAfter, open_player_cards},
						    {TFirst, open_board},
						    {TEnd, start_showdown}];
					   river->
						   [{TAfter, start_showdown}]
				   end
				]
		end,
	%?DEBUG("Timers=~p", [Timers]),
	#done{sd=SD,
	      cur=nobody,
	      broadcast=BCFixBatches,
	      timers=Timers}.

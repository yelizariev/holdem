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

-module(holdem_pregame).
-include("holdem.hrl").
-include("holdem_internal.hrl").
-export([init/3, start/2]).
-export([next_blind/1]).
%%% ===========================================
%%%   init/3
%%% -------------------------------------------

%% @doc вычисляет #preblinds (#pregame) и новое положение баттона
%%
%% Active - потенциальные игроки. Из них могут пропустить игру
%% те, которые подсели между МБ и ББ.
%%
%% Реализуется алгоритм doc/blinds.org
-spec init(#nogame{},
           Active::[#p{}],
           NoActive::[#p{}])
          -> {ok, seat(), #preblinds{}}.
init(#nogame{force_button=false,
             button_old =ButtonSeatOld,
             sb_seat_old=SBSeatOld_0,
             bb_seat_old=BBSeatOld_0},
     Active, NoActive)->
	SBSeatOld = not_member_to_undefined(Active, SBSeatOld_0),
	BBSeatOld = not_member_to_undefined(Active, BBSeatOld_0),
	if
		SBSeatOld=:=undefined,
		BBSeatOld=:=undefined->
			%% бывшие МБ и ББ не участвуют в игре (в том числе если кого
			%% то из них не было)
			[Button | _] = holdem_sort:right(Active, ButtonSeatOld),
			init(#nogame{force_button=Button#p.seat}, Active, NoActive);
		true->
			Sorted=
				if
					BBSeatOld=:=undefined->
						%% Бывший ББ не играет либо его не было, т.к. все
						%% отказались платить, например
						%%
						%% -0** -3** -6** [8] 9*
						%% or
						%% -0** -3** -6** 8* [9]
						holdem_sort:center(Active, SBSeatOld); % [ (SBSeatOld), SBSeatOld++, ...]
					true->
						holdem_sort:left(Active, BBSeatOld) % [ BBSeatOld--, (BBSeatOld), BBSeatOld++, ...]
				end,
			init2({ButtonSeatOld, SBSeatOld, BBSeatOld},
			      Sorted, NoActive)
	end;

init(#nogame{force_button=ButtonSeat}, Active, NoActive)->
	case holdem_sort:center(Active, ButtonSeat) of
		[Dealer, SB, BB | Tail]->
			init2({undefined, undefined, SB#p.seat},
			      [Dealer, SB, BB | Tail],
			      NoActive);
		[SB, BB] ->
			init2({undefined, undefined, undefined},
			      [BB, SB], NoActive)
	end.

%% @doc Active -- отсортированны [NewDealer, NewSB, NewBB | _] | [BB, SB]
-spec init2({ButtonSeatOld::seat(), SBSeatOld::seat(), BBSeatOld::seat()},
            Active::[#p{}], NoActive::[#p{}])->
		           {ok, seat(), #pregame{}}.
init2(_, [BB, SB], NoActive)->
	%% только два игрока
	{ok, SB#p.seat, #preblinds{
	            pregame = #pregame{sb=SB, bb=[BB], penalty=[]},
	            no_active_players = NoActive
	           }};
init2({ButtonSeatOld, SBSeatOld, BBSeatOld},
      [P|Players],
      NoActive)
  when SBSeatOld/=undefined, P#p.seat=/=SBSeatOld, % МБ не попал на баттон
       SBSeatOld=/=ButtonSeatOld % кроме случаев когда до этого играло 2 игрока
       ->
	%% игрок пропустит игру, т.к. на баттоне должен быть бывший МБ
	init2({ButtonSeatOld, SBSeatOld, BBSeatOld},
	      move_last_to_first(Players),
	      [P|NoActive]
	     );
init2({_ButtonSeatOld, _SBSeatOld, BBSeatOld},
      [Button|Players], NoActive)
  when BBSeatOld=:=undefined->
	%% не было ББ. МБлайндом станет первый заплативший
	{ok, Button#p.seat, #preblinds{
	                pregame = #pregame{sb=Players,
	                                   penalty=[Button]
	                                  },
	                no_active_players = NoActive
	               }};
init2({_ButtonSeatOld, _SBSeatOld, BBSeatOld},
      [Button, SB|Players], NoActive)
  when SB#p.seat=:=BBSeatOld->
	%% Стандарт. Бывший ББ платит МБ. Если откажется, то игра будет без МБ
	{ok, Button#p.seat, #preblinds{
	                pregame =  #pregame{sb=SB,
	                                    bb=Players,
	                                    penalty=[Button]
	                                    },
	                no_active_players = NoActive
	               }
		         };
init2(_, [Button|Players], NoActive)->
	%% SB#p.seat/=BBSeatOld, BBSeatOld/=undefined -- бывший ББ не участвует
	%% в игре
	%%
	%% Игра без МБ
	{ok, Button#p.seat,
	 #preblinds{pregame = #pregame{sb=false,
	                               bb=Players,
	                               penalty=[Button]
	                              },
	            no_active_players = NoActive
	           }
	}.


not_member_to_undefined(_Players, undefined)->
	undefined;
not_member_to_undefined(Players, Seat)->
	case lists:keymember(Seat, #p.seat, Players) of
		true-> Seat;
		false-> undefined
	end.
%%% ===========================================
%%%   start/2
%%% -------------------------------------------

%% @doc начать игру (сбор блайндов). Баттон уже переехал.
-spec start(#info{}, #preblinds{})->
		           #done{}.
start(I, #preblinds{pregame=G, no_active_players=NoActive})->
	% #info.Penalty
	Penalty=
		case  I#info.penalty_new_player of
			after_next_game->
				true;
			V->
				V
		end,
	SD = {I#info{players=NoActive,
	             banks=[#bank{}],
	             penalty_new_player=Penalty,
	             sb_seat=undefined,
	             bb_seat=undefined},
	      G},
	case I#info.ante of
		false->
			next_blind(SD);
		_->
			{ok, AnteRecord, NewSD} = holdem_ante:do(SD),
			#done{broadcast=[AnteRecord],
			      sd=NewSD,
			      timers=[{?TIMER(I, after_ante), next_blind}]
			     }
	end.
%%% ===========================================
%%%   next_blind
%%% -------------------------------------------

%% @doc OLD: используется после вызова pay_blind, not_pay_blind, а также в
%% start_game
-spec next_blind(sd())-> #done{}.
next_blind({I, Pre = #pregame{sb=SBValue, bb=BBValue,
                              penalty=Penalty}})->
	case {SBValue, BBValue, Penalty} of
		{SB, _, _} when is_record(SB, p)->
			req_blind({I, Pre}, SB, sb);
		{[SB|_], _, _} ->
			req_blind({I, Pre}, SB, sb);
		{false, [_], []}->
			%% только два игрока и МБ отказался
			holdem_blind:death({I, Pre});
		{_, [BB|_], _}->
			req_blind({I, Pre}, BB, bb);
		{_, [], _}->
			%% заменим [] на false
			next_blind({I, Pre#pregame{bb=false}});
		{[], _, _}->
			%% МБ мог быть первый заплативший, но никто не согласился и
			%% остался только баттон
			holdem_blind:death({I, Pre#pregame{bb=false}});
		{true, false, []} when length(Pre#pregame.ingame)=:=1->
			%% только два игрока и ББ отказался
			holdem_blind:death({I, Pre});
		{false, false, _}->
			%% остался только баттон
			holdem_blind:death({I, Pre});
		{_, _, [P|_Tail]}
		  when P#p.skipsb;P#p.skipbb=/=false->
			req_blind({I,Pre}, P, penalty);
		{_, _, [P|Tail]}->
			next_blind({I, Pre#pregame{ingame=
			                           [P|Pre#pregame.ingame],
			                           penalty=Tail}});
		_ ->
			deal_cards({I, Pre})
	end.

-spec req_blind(sd(), #p{}, sb|bb|penalty) ->
	#done{}.
req_blind({I = #info{autoblinds=Autoblinds,
                    autoplayer_strategy=Strategy}, G},
          P, Type)->
	PayBlindType=
		case {Type, P#p.skipsb, P#p.skipbb} of
			{bb, true, _}->
				sbb;
			{bb, false, _}->
				bb;
			{sb, _, false}->
				sb;
			{sb, _, _} ->
				bb;
			{penalty, _, first_game_penalty}->
				%% игрок еще не играл
				bb;
			{penalty, true, false}
			  when P#p.seat=:=I#info.button->
				%% отлавливаем случаи (см.3)
				%% [0] -3* 6** 8
				%% 0 [3]# 6* 8**

				%% но пропускаем случаи (см. 0)
				%% -0* -3** 6** 8 [9]
				%% 0### -[3]### 6* 8** 9
				sb;
			{penalty, false, true}
			  when P#p.seat=/=I#info.button->
				%% пропускаем случаи (см.3)
				%% -0* -3** 6** 8 [9]
				%% 0### -[3]### 6* 8** 9
				bb;
			{penalty, _, _} ->
				sbb
		end,
	AutoPlayer = lists:member(P#p.seat, I#info.autoplayers),
	NewG=G#pregame{cur=PayBlindType, cur_seat=P#p.seat},
	if
		Autoblinds;
		AutoPlayer, Strategy=:=blind_check_fold->
			{ok, PayBlind, NewSD} = holdem_blind:pay({I,NewG}),
			#done{broadcast=[PayBlind],
			      timers=[{?TIMER(I, after_autoblinding), next_blind}],
			      sd=NewSD};
		true->
			{TimerType, TimerDelay}=
				case ?TIMER(I, blind_prewarning) of
					0-> {warning, ?TIMER(I, blind_warning)};
					Delay -> {prewarning, Delay}
				end,
			Timer =
				if
					TimerType==warning->
						TimerDelay;
					true->
						false
				end,
			NowMove = #now_move{seat=P#p.seat,
			                    mseconds = Timer},
			{Chips, IsAllin} = holdem_blind:chips(I, P, PayBlindType),
			ReqBlind = #req_blind{seat=P#p.seat, type=PayBlindType, chips=Chips, is_all_in=IsAllin},
			#done{cur = {P#p.seat, pay_blind},
			      cur_timer={TimerType, TimerDelay},
			      broadcast = [NowMove, {P#p.seat, ReqBlind}],
			      sd={I, NewG#pregame{cur_req_blind=ReqBlind,
			                         cur_timer={holdem:unixtime(), {TimerType, TimerDelay}}}}
			     }
	end.

%%% ===========================================
%%%   deal_cards
%%% -------------------------------------------
-spec deal_cards(sd())->
	#done{}.
deal_cards({I, Pre})->
	{DealingCards, NotAllin, Allin, BoardCards}=
		deal_cards(I#info.button,
		           Pre#pregame.ingame,
		           I#info.allin),
	Wait = holdem_sort:right(NotAllin, I#info.bb_seat),
	SD = {I#info{allin=Allin},
	      #game{board_cards=BoardCards,
	            cur_bet=I#info.bb,
	            delta_raise=I#info.bb,
	            wait=Wait} 
	     },
	%% DealingCards::[{Seat, Card1, Card2}]
	Seats = [Seat || {Seat, _Card1, _Card2} <- DealingCards],
	Broadcast = [{viewers, #deal_cards_viewer{seats=Seats}}
	            | [{Seat, #deal_cards_player{seat=Seat, card1=Card1, card2=Card2, seats=Seats}}
	               || {Seat, Card1, Card2} <- DealingCards]],
	#done{broadcast=Broadcast,
	      cur=nobody,
	      timers=[{?TIMER(I, after_dealing), preflop}],
	      sd=SD}.

%% @spec()->{DealingCards, Players1, Players2, BoardCards}
deal_cards(Button, Players1, Players2)->
	[BC1, BC2, BC3, BC4, BC5 | Deck]=
		holdem_cards:shuffle(5+2*(length(Players1)+length(Players2))),
	BoardCards = [BC1, BC2, BC3, BC4, BC5],
	{PP1, DeckTail} =
		update_cards(Players1, Deck),
	{PP2, _} =
		update_cards(Players2, DeckTail),
	SortedPlayers = holdem_sort:right(PP1++PP2, Button),
	{[{Seat, C1, C2} || #p{card1=C1, card2=C2, seat=Seat} <-SortedPlayers],
	 PP1, PP2,
	 BoardCards}.

%% spec([#p], [card()]) -> {[#p],DeckTail}
update_cards(Players, Deck)->
	update_cards(Players, Deck, []).
update_cards([P|Players], [C1, C2 | Deck],
             AccPlayers)->
	NewP = P#p{card1=C1, card2=C2},
	update_cards(Players, Deck, [NewP|AccPlayers]);
update_cards([], Deck, AccPlayers) ->
	{AccPlayers, Deck}.

%%% ===========================================
%%%
%%% -------------------------------------------
%% @spec ([X1, X2, ..., X(N-1), XN])->
%%        [XN, X1, X2, ... X(N-1)]
move_last_to_first(List)->
	[Last | Tail] = lists:reverse(List),
	[Last | lists:reverse(Tail)].

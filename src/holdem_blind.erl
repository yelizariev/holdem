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

%%% @doc начало игры, уплата блайднов 

%%% Если применять анте без autoblinds, то возврать фишек в случае
%%% #blind_death не будет учитывать анте; кроме того возможны нарушения
%%% правил определения позиции блайндов в случае алл-ни на анте
-module(holdem_blind).
-include("holdem.hrl").
-include("holdem_internal.hrl").

-export([pay/1,
         not_pay/1,
         chips/3,
         death/1]).


%% @spec (#info, #p, blind_type())-> {Chips, IsAllin}
chips(#info{sb=SB, bb=BB}, #p{stack=Stack}, PayBlindType)->
	PayBlindChips=
		case PayBlindType of
			sb-> SB;
			bb -> BB;
			sbb -> SB+BB
		end,
	case Stack =< PayBlindChips of
		false->
			{PayBlindChips, false};
		true->
			{Stack, true}
	end.


%% @doc перемещаем игрока в #pregame.ingame (или #info.banks),
%% снимаем отметки skipsb, skipbb. Перемещаем фишки из стека в ставку.
-spec pay(sd())->
	{ok, #pay_blind{}, sd()}.
pay({I0, G0})->
	{I, G, P}=
		case {G0#pregame.sb, G0#pregame.bb, G0#pregame.penalty} of
			{SB, _, _} when is_record(SB, p)->
				{I0#info{sb_seat=SB#p.seat},
				 G0#pregame{sb=true}, SB};
			{[SB|Tail], _, _} ->
				{I0#info{sb_seat=SB#p.seat},
				 G0#pregame{sb=true, bb=Tail}, SB};
			{_, [BB|Tail], _} ->
				{I0#info{bb_seat=BB#p.seat},
				 G0#pregame{bb=true,
				              penalty=Tail++G0#pregame.penalty},
				 BB};
			{_, _, [Player|Tail]}->
				{I0,
				 G0#pregame{penalty=Tail},
				 Player}
		end,
	{Chips, IsAllin} = chips(I, P, G#pregame.cur),
	{_DeltaBatches, NewBatches}=
		holdem_batches:bet(I#info.allin, Chips, [0], length(I#info.banks)-1),
	%% (DeltaBatches==NewBatches)
	NewP = P#p{skipsb  = false,
	           skipbb  = false,
	           last_action=G#pregame.cur,
	           bet     = Chips,
	           batches = NewBatches,
	           stack   = P#p.stack-Chips},
	Allin =
		if
			IsAllin->
				length(NewBatches)+length(I#info.banks)-2;
			true->
				false
		end,
	PayBlind = #pay_blind{type = G#pregame.cur,
	                      data = #bet_data{
	                        seat = P#p.seat,
	                        batches = NewBatches,
	                        allin = Allin
	                       }},
	SD =
		case IsAllin of
			false->
				{I, G#pregame{ingame=[NewP|G#pregame.ingame],
				              cur_seat=undefined}};
			true->
				{I#info{allin=[NewP|I#info.allin]},
				 G#pregame{cur_seat=undefined}}
		end,
	{ok, PayBlind, SD}.

-spec not_pay(sd())->
	{ok, #not_pay_blind{}, sd()}.
%% @spec () -> {T, NotPayBlind}
%% @doc перемещает игрока в #info.players, делает отметки skipsb, skipbb
not_pay({I0, G0})->
	{I, G, P, Type} =
		case {G0#pregame.sb, G0#pregame.bb, G0#pregame.penalty} of
			{SB, _, _} when is_record(SB, p)->
				{I0#info{sb_seat=undefined},
				 G0#pregame{sb=false},
				 SB, sb};
			{[SB|Tail], _, _} ->
				{I0#info{sb_seat=undefined},
				 G0#pregame{sb=Tail},
				 SB, sb};
			{_, [BB|Tail], _} ->
				{I0#info{bb_seat=undefined},
				 G0#pregame{bb=Tail},
				 BB, bb};
			{_, _, [Player|Tail]}->
				{I0,
				 G0#pregame{penalty=Tail},
				 Player, penalty}
		end,
	%% Type = sb | bb | penalty
	NotPayBlind = #not_pay_blind{seat=P#p.seat},
	NewP=
		case Type of
			sb->P#p{skipsb=true};
			bb->P#p{skipbb=true};
			penalty
			  when G0#pregame.cur=:=sbb->
				P#p{skipsb=true, skipbb=true};
			penalty -> P
		end,
	{ok, NotPayBlind,
	 {I#info{players=[NewP|I#info.players]}, G}}.

-spec death(sd())->
		           #done{}.
death({I, #pregame{ingame=InGame,
                   penalty=Penalty,
                   bb=BBValue}})->
	{Player, ReturnBlind} = 
		case {BBValue, Penalty, InGame} of
			{[P], [], []}->
				%% два игрока. МБ не заплатил
				{P, false};
			{false, [], [P = #p{seat=Seat, bet=Chips, stack=Stack}]}->
				%% два игрока. ББ не заплатил
				NewP=P#p{bet=0, batches=[],
				         stack=Stack+Chips},
				{NewP, {Seat, Chips}};
			{false, [P], []}->
				%% >2 игроков. Никто ничего не внёс
				{P, false}
		end,
	#done{sd= {I#info{players=[Player|I#info.players],
	                  button={new, I#info.button}},
	           nogame},
	      cur=nobody,
	      broadcast=[#blind_death{return_blind=ReturnBlind}]
	     }.

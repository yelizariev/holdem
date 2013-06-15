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

-module(holdem_ante).
-export([do/1]).

-include("holdem.hrl").
-include("holdem_internal.hrl").

-define(ADD_ALLIN(I, P), I#info{allin=[P | I#info.allin]}).



%% @doc обновляет поле #p.stack, #info.allin, #info.banks
-spec do(sd())->
		      {ok, #ante{}, sd()}.
do(T = {#info{ante=Ante}, _})->
	%%#pregame{sb=SB, bb=BB, penalty=Penalty, ingame=[]}
	{{I, Pre}, Seats, IsAllin} = ante(sb, Ante, T, false, []),
	case IsAllin of
		false->
			{ok,
			 #ante{value={I#info.ante, Seats}},
			 {I#info{banks=[#bank{chips=length(Seats)*Ante}]},
			  Pre}
			};
		true->
			{NewSD, Value} =
				holdem_batches:collect_ante_batches({I, Pre}, Seats),
			{ok, #ante{value=Value}, NewSD}
	end.

%% @spec(CurPregame, Ante, T, IsAllin, List) -> {T, Seats, IsAllin}
%%
%% CurPregame = sb | bb | penalty
%% IsAllin - есть ли хотя бы один алл-ин игрок
%% Seats = [seat()] - список не алл-ин игроков
%%
%% @doc уменьшает #p.stack (кроме аллин игроков)
ante(sb, Ante, {I, Pre = #pregame{sb=P}}, IsAllin, Seats)
  when is_record(P, p),
       P#p.stack>Ante ->
	ante(bb, Ante,
	     {I, Pre#pregame{sb=P#p{stack=P#p.stack-Ante}}},
	     IsAllin,
	     [P#p.seat| Seats]);
ante(sb, Ante, {I, Pre = #pregame{sb=P}}, _IsAllin, Seats)
  when is_record(P, p) ->
	ante(bb, Ante,
	     {?ADD_ALLIN(I, P), Pre#pregame{sb=true}},
	     true,
	     Seats);
ante(sb, Ante, {I, Pre = #pregame{sb=[P|SBTail]}}, IsAllin, Seats)
  when P#p.stack>Ante ->
	ante(bb, Ante,
	     {I, Pre#pregame{sb=P#p{stack=P#p.stack-Ante},
	                     bb=SBTail}},
	     IsAllin,
	     [P#p.seat| Seats]);
ante(sb, Ante, {I, Pre = #pregame{sb=[P|SBTail]}}, _IsAllin, Seats)->
	ante(bb, Ante,
	     {?ADD_ALLIN(I, P), Pre#pregame{sb=true,
	                                    bb=SBTail}},
	     true,
	     Seats);
ante(sb, Ante, T, IsAllin, Seats)->
	ante(bb, Ante, T, IsAllin, Seats);


ante(bb, Ante, {I, Pre = #pregame{bb=P}}, IsAllin, Seats)
  when is_record(P, p),
       P#p.stack>Ante ->
	ante(penalty, Ante,
	     {I, Pre#pregame{bb=P#p{stack=P#p.stack-Ante}}},
	     IsAllin,
	     [P#p.seat| Seats]);
ante(bb, Ante, {I, Pre = #pregame{bb=P}}, _IsAllin, Seats)
  when is_record(P, p) ->
	ante(penalty, Ante,
	     {?ADD_ALLIN(I, P), Pre#pregame{bb=true}},
	     true,
	     Seats);
ante(bb, Ante, {I, Pre = #pregame{bb=[P|BBTail]}}, IsAllin, Seats)
  when P#p.stack>Ante ->
	ante(penalty, Ante,
	     {I, Pre#pregame{bb=P#p{stack=P#p.stack-Ante},
	                     penalty=BBTail++Pre#pregame.penalty}},
	     IsAllin,
	     [P#p.seat| Seats]);
ante(bb, Ante, {I, Pre = #pregame{bb=[P|SBTail]}}, _IsAllin, Seats)->
	ante(penalty, Ante,
	     {?ADD_ALLIN(I, P),
	      Pre#pregame{bb=true,
	                  penalty=SBTail++Pre#pregame.penalty}},
	     true,
	     Seats);
ante(bb, Ante, T, IsAllin, Seats)->
	ante(penalty, Ante, T, IsAllin, Seats);

ante(penalty, Ante, {I, Pre}, IsAllin, Seats)->
	{Penalty, NewSeats, NewAllin, NewIsAllin} =
		ante_penalty(Pre#pregame.penalty,
		             Ante,
		             IsAllin,
		             I#info.allin,
		             Seats, []),
	{{I#info{allin=NewAllin}, Pre#pregame{penalty=Penalty}},
	 NewSeats, NewIsAllin}.

%% @spec (Penalty, Ante, IsAllin, AllinAcc, Seats, PenaltyAcc) ->
%% {Penalty, Seats, Allin, IsAllin}
ante_penalty([P|Penalty], Ante, IsAllin, AllinAcc, Seats, PenaltyAcc)
  when P#p.stack > Ante->
	ante_penalty(Penalty, Ante, IsAllin, AllinAcc,
	             [P#p.seat | Seats],
	             [P#p{stack=P#p.stack-Ante}| PenaltyAcc]);
ante_penalty([P|Penalty], Ante, _IsAllin, AllinAcc, Seats, PenaltyAcc)
  when P#p.stack > Ante->
	ante_penalty(Penalty, Ante, true,
	             [P | AllinAcc],
	             Seats,
	             PenaltyAcc);
ante_penalty([], _Ante, IsAllin, AllinAcc, Seats, PenaltyAcc)->
	{PenaltyAcc, Seats, AllinAcc, IsAllin}.



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

%%% @doc you_move, now_move, premove
-module(holdem_move).
-include("holdem.hrl").
-include("holdem_internal.hrl").

-export([pre/2,
         you/2]).

-spec pre(Wait::[#p{}], CurBet::chips())->
		         {ok, [#precall{} | #precheck{}], [#p{}]}.
pre(Wait, CurBet)->
	premove(Wait, CurBet, [], []).
%% @spec(Wait, CurBet,
%%       [#precall | #precheck],
%%       [#p])
premove([P | Wait], CurBet, PreMove, Updated)
  when P#p.premove=:=CurBet->
	premove(Wait, CurBet, PreMove, [P|Updated]);
premove([P | Wait], CurBet, PreMove, Updated)
  when P#p.bet=:=CurBet->
	PreCheck={P#p.seat, #precheck{}},
	premove(Wait, CurBet,
	        [PreCheck | PreMove],
	        [P#p{premove=CurBet}|Updated]);
premove([P=#p{seat=Seat, bet=PBet, stack=Stack} | Wait],
        CurBet, PreMove, Updated)->
	{CallAdd, IsAllin}=
		case CurBet>PBet+Stack of
			false->
				{CurBet-PBet, false};
			true->
				{Stack, true}
		end,
	PreCall={Seat, #precall{call_add=CallAdd, is_all_in=IsAllin}},
	premove(Wait, CurBet,
	        [PreCall | PreMove],
	        [P#p{premove=CurBet}|Updated]);
premove([], _, PreMove, Updated)->
	{ok, PreMove, lists:reverse(Updated)}.

%% @doc return #you_move{}, MinRaise, MaxRaise
-spec you(sd(), #p{}) ->
		      {ok, #you_move{}, chips(), chips()}.
you({I, #game{cur_bet=CurBet,
                   delta_raise=DeltaRaise,
                   wait=Wait,
                   done=Done}},
         #p{bet=PBet,
            stack=Stack,
            last_action=LastAction,
            seat=Seat})->
	CallAdd=CurBet-PBet, 
	MaxBet=PBet+Stack,
	CurBet_DeltaRaise = CurBet+DeltaRaise,
	{Chips, MinRaise, MaxRaise}=
		if
			CallAdd >= Stack ->
				{[Stack, 0],
				 1, 0};
			length(Wait)=:=1, Done=:=[];
			0 < CallAdd, CallAdd < DeltaRaise, LastAction=/=false->
				{[CallAdd],
				 1, 0};
			CurBet_DeltaRaise >=  MaxBet->
				{[CallAdd, MaxBet],
				 MaxBet, MaxBet};
			true ->
				{[CallAdd, CurBet_DeltaRaise, MaxBet, I#info.bb],
				 CurBet_DeltaRaise, MaxBet}
		end,
	{ok, {Seat, #you_move{chips=Chips}}, MinRaise, MaxRaise}.

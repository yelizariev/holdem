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

-module(holdem_sort).
-include("holdem.hrl").
-include("holdem_internal.hrl").
-export([left/2,
        center/2,
        right/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% ===========================================
%%%   API
%%% -------------------------------------------
%% @spec(Players, Seat)-> Sorted
%% Players, Sorted = [#p]
%% Seat++ означает игрока справа от Seat
%% (Seat) означает, что этот игрок может остутствовать

%% [Seat--, (Seat), Seat++, ...]
left(Players, Seat)->
	left(sort(Players), Seat, []).

-ifdef(EUNIT).
left_test_()->
	[?_assertEqual([5,1,2,3,4],  s(left(p([1,2,3,4,5]),  1))),
	 ?_assertEqual([4, 2],       s(left(p([2,4]),        2))),
	 ?_assertEqual([2, 4],       s(left(p([2,4]),        3))),
	 ?_assertEqual([2, 4],       s(left(p([2,4]),        4))),
	 ?_assertEqual([8, 2, 4, 6], s(left(p([2, 4, 6, 8]), 1))),
	 ?_assertEqual([8, 2, 4, 6], s(left(p([2, 4, 6, 8]), 2))),
	 ?_assertEqual([2, 4, 6, 8], s(left(p([2, 4, 6, 8]), 3))),
	 ?_assertEqual([2, 4, 6, 8], s(left(p([2, 4, 6, 8]), 4))),
	 ?_assertEqual([6, 8, 2, 4], s(left(p([2, 4, 6, 8]), 8))),
	 ?_assertEqual([8, 2, 4, 6], s(left(p([2, 4, 6, 8]), 9)))
	].
-endif.


%% SortedCenter [(Seat), Seat++, Seat++2, ...]
center([], _Seat)->
	[];
center(Players, Seat)->
	center(sort(Players), Seat, []).

-ifdef(EUNIT).
center_test_()->
	[
	 ?_assertEqual([1,2,3,4,5],  s(center(p([1,2,3,4,5]),  1))),
	 ?_assertEqual([2, 4],       s(center(p([2, 4]),       2))),
	 ?_assertEqual([4, 2],       s(center(p([2, 4]),       3))),
	 ?_assertEqual([4, 2],       s(center(p([2, 4]),       4))),
	 ?_assertEqual([2, 4, 6, 8], s(center(p([2, 4, 6, 8]), 1))),
	 ?_assertEqual([2, 4, 6, 8], s(center(p([2, 4, 6, 8]), 2))),
	 ?_assertEqual([4, 6, 8, 2], s(center(p([2, 4, 6, 8]), 3))),
	 ?_assertEqual([4, 6, 8, 2], s(center(p([2, 4, 6, 8]), 4))),
	 ?_assertEqual([8, 2, 4, 6], s(center(p([2, 4, 6, 8]), 8))),
	 ?_assertEqual([2, 4, 6, 8], s(center(p([2, 4, 6, 8]), 9)))
	].
-endif.

%% SortedRight [Seat++, Seat++2, Seat++3, ...]
%% 
right(Players, Seat)->
	right(sort(Players), Seat, []).

-ifdef(EUNIT).
right_test_()->
	[
	 ?_assertEqual([2,3,4,5,1],  s(right(p([1,2,3,4,5]),  1))),
	 ?_assertEqual([4, 2],       s(right(p([2, 4]),       2))),
	 ?_assertEqual([4, 2],       s(right(p([2, 4]),       3))),
	 ?_assertEqual([2, 4],       s(right(p([2, 4]),       4))),
	 ?_assertEqual([2, 4, 6, 8], s(right(p([2, 4, 6, 8]), 1))),
	 ?_assertEqual([4, 6, 8, 2], s(right(p([2, 4, 6, 8]), 2))),
	 ?_assertEqual([4, 6, 8, 2], s(right(p([2, 4, 6, 8]), 3))),
	 ?_assertEqual([6, 8, 2, 4], s(right(p([2, 4, 6, 8]), 4))),
	 ?_assertEqual([2, 4, 6, 8], s(right(p([2, 4, 6, 8]), 8))),
	 ?_assertEqual([2, 4, 6, 8], s(right(p([2, 4, 6, 8]), 9)))
	].
-endif.



%%% ===========================================
%%%   Doing
%%% -------------------------------------------
%% @spec()
%% Acc - игроки, находящиеся раньше, чем игрок, которого ищем
left([P1, P2 | Tail], Seat, Acc)
  when P1#p.seat<Seat, P2#p.seat>=Seat->
	[P1, P2 | Tail]++lists:reverse(Acc);
left([P1, P2 | Tail], Seat, Acc)->
	left([P2|Tail], Seat, [P1|Acc]);
left([P1], _Seat, Acc) ->
	%% 1) P1 < Seat
	%% 2) Все P >= Seat
	[P1 | lists:reverse(Acc)].

center([P1, P2 | Tail], Seat, Acc)
  when P2#p.seat=<Seat->
	center([P2|Tail], Seat, [P1|Acc]);
center([P1, P2 | Tail], Seat, Acc)
  when P1#p.seat>=Seat->
	%% P1 >= Seat, P2 > Seat, 
	[P1, P2 | Tail]++lists:reverse(Acc);
center([P1, P2 | Tail], _Seat, Acc)->
	%% P1 < Seat, P2 > Seat
	[P2 | Tail]++lists:reverse([P1|Acc]);
center([P1], Seat, Acc)
  when P1#p.seat =:= Seat->
	[P1|lists:reverse(Acc)];
center([P1], _Seat, Acc)->
	%% P1 < Seat
	lists:reverse([P1|Acc]).

right([P1 | Tail], Seat, Acc)
  when P1#p.seat>Seat->
	[P1 | Tail]++lists:reverse(Acc);
right([P1 | Tail], Seat, Acc)->
	right(Tail, Seat, [P1 | Acc]);
right([], _Seat, Acc)->
	lists:reverse(Acc).


sort(Players)->
	lists:keysort(#p.seat, Players).
%%% ===========================================
%%%   test tool
%%% -------------------------------------------
-ifdef(EUNIT).
p(List)->
	[#p{seat=S} || S<-List].
s(Players)->
	[S || #p{seat=S} <- Players].
-endif.

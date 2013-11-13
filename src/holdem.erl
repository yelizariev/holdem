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

%%% @doc Реализация игрового процесса игры "безлимитный холдем".
%%%
%%% @headerfile "../include/holdem.hrl"
-module(holdem).
-behaviour(gen_server).
-include("holdem.hrl").
-include("holdem_internal.hrl").
-define(CALL_TIMEOUT, infinity).

-export([app_start/0, app_stop/0]).

-export([start/1, stop/1, stop/2]).
-export([start_link/1, init/1]).
-export([
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).
-export([next/2,
         boast/2]).
-export([autoplayer/3,
         autostart/2,
         button/2,
         seat/4,
         unseat/2,
         get_stack/2,
         add_to_stack/3
        ]).
-export([snapshot/2]).
-export([debug_update_cards/3]).
-export([unixtime/0]).
%%% ===========================================
%%%   Application API
%%% -------------------------------------------
app_start()->
	lager:start(),
	lager:info("PWD: ~p", [os:cmd("pwd")]),
	application:start(holdem, permanent).

app_stop()->
	application:stop(holdem).
%%% ===========================================
%%%   start\stop API
%%% -------------------------------------------
-spec start(#holdem_options{}) -> any().
start(Arg)->
	holdem_sup:start_child(Arg).

stop(Pid)->
	stop(Pid, normal).
stop(Pid, Reason)->
	gen_server:call(Pid, {stop, Reason}, ?CALL_TIMEOUT). 

%%% ===========================================
%%%   Init
%%% -------------------------------------------
start_link(InitArgs)->
	gen_server:start_link(?MODULE, InitArgs, [
	                                          %{debug, [trace]}
	                                         ]).

init(#holdem_options{autoblinds = Autoblind,
                     autoplayer_strategy=Strategy,
                     autostart=Autostart,
                     blinds= Blinds,
                     max_players=MaxPlayers,
                     penalty = Penalty,
                     rake = Rake,
                     callback=Callback,
                     timers=Timers
                    })->
	#blinds{sb=SB, bb=BB, ante=Ante}=Blinds,
	{ok, {#info{sb=SB,
	            bb=BB,
	            ante=Ante,

	            max_players=MaxPlayers,

	            autoblinds = Autoblind,
	            autoplayer_strategy=Strategy,
	            autostart=Autostart,
	            penalty_new_player = case Penalty of
		                                 true-> after_next_game;
		                                 false->false
	                                 end,
	            rake = Rake,

	            callback=Callback,
	            timers=Timers
	           },
	      #nogame{}}}.

%%% ===========================================
%%%   API - изменения, способные привести к запуску игры
%%% -------------------------------------------

%% @doc увеличить стек игрока.
-spec add_to_stack(pid(), seat(), {chips(), chips(), chips()})->
		           {ok, chips(), #holdem{}} | {error, any()}.
add_to_stack(Pid, Seat, {AddChips, MinStack, MaxStack})->
	gen_server:call(Pid, {add_to_stack, Seat, {AddChips, MinStack, MaxStack}}, ?CALL_TIMEOUT).

-spec get_stack(pid(), seat())->
		           {ok, chips(), boolean()} | {error, not_found}.
get_stack(Pid, Seat)->
	gen_server:call(Pid, {get_stack, Seat}, ?CALL_TIMEOUT).

%% @doc игрок присаживается за стол. Функция не проверяет занято ли
%% уже место. не превосходит ли seat() max_players-1. Если это первый
%% игрок, то #nogame.force_button задается значение Seat.
%%
%% PlayerInfo -- передается в #holdem_snapshot.players[i].info
-spec seat(pid(), seat(), chips(), PlayerInfo::any())->
		          #holdem{}.
seat(Pid, Seat, Stack, PlayerInfo)->
	gen_server:call(Pid, {seat, Seat, Stack, PlayerInfo}, ?CALL_TIMEOUT).

%% @doc включить\выключить режим автоигрока. Запрос клиенту не
%% отправляет, решение о ходе принимается автоматически согласно
%% #holdem_options.autoplayer
-spec autoplayer(pid(), seat(), boolean())->
		          #holdem{}.
autoplayer(Pid, Seat, AutoPlayer)->
	gen_server:call(Pid, {autoplayer, Seat, AutoPlayer}, ?CALL_TIMEOUT).

%% @doc Задать значение параметра autostart и начать игру, если
%% Autostart == true и игра еще не начата
-spec autostart(pid(), Autostart :: boolean())->
		               #holdem{}.
autostart(Pid, Autostart)->
	gen_server:call(Pid, {autostart, Autostart}, ?CALL_TIMEOUT).

%%% ===========================================
%%%   API
%%% -------------------------------------------
%% @doc игрок встаёт из-за стола. Если никого не осталось, то буттон
%% переезжает в центр стола
-spec unseat(pid(), seat()) ->
		            {ok, stack(), #holdem{}} |
		            {ok, player_in_game, #holdem{}}.
unseat(Pid, Seat)->
	gen_server:call(Pid, {unseat, Seat}, ?CALL_TIMEOUT).

%% @doc После старта новой игры баттон не будет переезжать к
%% следующему игроку, останется на месте и блайнды будут определятся
%% исходя из положения баттона. Используется только вне игры.
-spec button(pid(), seat())-> ok.
button(Pid, Seat)->
	gen_server:call(Pid, {button, Seat}).

% @doc необходимо вызывать сразу после того, как уплачены блайнды
-spec debug_update_cards(pid(), [card()], [{seat(), card(), card()}])->
		                        ok.
debug_update_cards(Pid, Board, Players)->
	gen_server:call(Pid, {debug_update_cards, Board, Players}, ?CALL_TIMEOUT).
%%% ===========================================
%%%   API - game
%%% -------------------------------------------
%% Используется когда игра окончена и случилось так, что игрок не
%% обязан открывать свои карты; игрок получает запрос req_boast() и
%% решает открыть карты отправляет boast()
-spec boast(pid(), seat())->
		          #holdem{}.
boast(Pid, Seat)->
	gen_server:call(Pid, {boast, Seat}, ?CALL_TIMEOUT).

%% @doc seat() -- кто может присылать holdem_in
-spec next(pid(), holdem_in())->
		          #holdem{}.
next(Pid, HoldemIn)->
	gen_server:call(Pid, {next, HoldemIn}).
%%% ===========================================
%%%   API - snapshot
%%% -------------------------------------------
%% @doc broadcast -- может содержать #you_move{}, #now_move{}, #req_blind
-spec snapshot(pid(), Owner::seat()|false)->
		              {ok, #holdem_snapshot{}, broadcast()}.
snapshot(Pid, Owner)->
	gen_server:call(Pid, {snapshot, Owner}, ?CALL_TIMEOUT).



%%% ===========================================
%%%   Callbacks
%%% -------------------------------------------
handle_call({add_to_stack, Seat, {AddChips, MinStack, MaxStack}}, _, SD) ->
	case holdem_start:add_to_stack(SD, Seat, {AddChips, MinStack, MaxStack}) of
		{error, Reason} ->
			{reply, {error, Reason}, SD};
		{ok, NewStack, Result}->
			{reply, Holdem, NewSD} = reply(Result),
			{reply, {ok, NewStack, Holdem}, NewSD}
	end;
handle_call({get_stack, Seat}, _, SD) ->
	R = holdem_snapshot:get_stack(SD, Seat),
	{reply, R, SD};
handle_call({unseat, Seat}, _, SD) ->
	{ok, Result, Done} = holdem_unseat:do(SD, Seat),
	{Holdem, NewSD} = timers_done(Done),
	{reply, {ok, Result, Holdem}, NewSD};
handle_call({autoplayer, Seat, AutoPlayer}, _, {I,G}) ->
	SD=
		if
			AutoPlayer==false->
				holdem_unseat:player_reconnected({I,G}, Seat);
			true->
				{I,G}
		end,
	Done = holdem_start:autoplayer(SD, Seat, AutoPlayer),
	reply(Done);
handle_call({boast, Seat}, _, {I, G})
  when is_record(G, fgame) ->
	reply(holdem_showdown:boast({I,G}, Seat));
handle_call({boast, _Seat}, _, SD)->
	{reply, #holdem{}, SD};
handle_call({next, HoldemIn}, _, SD) ->
	reply(holdem_next:holdem_in(stop_waiting_timer(SD), HoldemIn));
handle_call({snapshot, Owner}, _, SD) ->
	Snapshot = holdem_snapshot:do(SD, Owner),
	{reply, Snapshot, SD};
handle_call({seat, Seat, Stack, PlayerInfo}, _, SD) ->
	reply(holdem_start:seat(SD, Seat, Stack, PlayerInfo));
handle_call({button, Seat}, _, {I, G})
  when is_record(G, nogame), G#nogame.timer=:=false -> 
	{reply, ok, {I, G#nogame{force_button=Seat}}};
handle_call({autostart, Autostart}, _, SD) ->
	reply(holdem_start:autostart(SD, Autostart));
handle_call({debug_update_cards, Board, Players}, _, {I,G})->
	F = fun (P)->
			    {_, C1,C2}=lists:keyfind(P#p.seat, 1, Players),
			    P#p{card1=C1, card2=C2}
	    end,
	{reply, ok, {I, G#game{board_cards=Board,
	                       wait=[ F(P) ||P<-G#game.wait]}}};
handle_call({stop, Reason}, _, SD) ->
	{stop, Reason, ok, SD}.

handle_info({timer, OnTimer, NextTimers},
            {I=#info{callback={M, F, Args}}, G})->
	%?DEBUG("OnTimer=~p", [OnTimer]),
	{Holdem, SD} = ontimer({I,G}, OnTimer),
	{NewHoldem, NewSD} = timers(NextTimers, Holdem, SD),
	erlang:apply(M, F, [NewHoldem | Args]),
	{noreply, NewSD};
handle_info({timeout, TimerRef, waiting_timer},
            {I = #info{waiting={prewarning, Ref},
                       callback={M, F, Args}},
             G})
  when TimerRef=:=Ref,
       is_record(G,pregame) orelse
       is_record(G, game) andalso G#game.wait=/=[] ->
	{Seat, Delay} =
		case G of
			#pregame{cur_seat=CurSeat}->
				{CurSeat, ?TIMER(I, blind_warning)};
			#game{wait=[P|_]}->
				{P#p.seat, ?TIMER(I, move_warning)}
		end,
	Holdem = #holdem{
	  broadcast=[#now_move{seat=Seat, mseconds=Delay}]
	 },
	erlang:apply(M, F, [Holdem | Args]),
	{noreply, start_waiting_timer({I,G}, warning, Delay)};
handle_info({timeout, TimerRef, waiting_timer},
            {I = #info{waiting={warning, Ref}}, G})
  when TimerRef=:=Ref,
       is_record(G, pregame)->
	reply(holdem_next:holdem_in({I,G}, {pay_blind, false}));
handle_info({timeout, TimerRef, waiting_timer},
            {I = #info{waiting={warning, Ref},
                       callback={M, F, Args}}, G})
  when TimerRef=:=Ref,
       is_record(G, game)->
	Done=holdem_next:holdem_in({I,G}, {move, autocheck}),
	Seat = element(1, G#game.cur_you_move),
	NewDone = ?EXTEND_BROADCAST({Seat, #wakeup{}}, Done),
	{Holdem, NewSD} = timers_done(NewDone),
	erlang:apply(M, F, [Holdem | Args]),
	{noreply, NewSD};
handle_info({timeout, _TimerRef, waiting_timer}, SD)->
	{noreply, SD}.
	%case ontimer({I,G}, Action#execute) of
	%	false->
	%		erlang:apply(M, F, [ HO | Args]);
	%		{noreply, {I,G}};
	%	{ok, HoldemOut, NewSD}->
	%		erlang:apply(M, F, [ HO ++ HoldemOut | Args]);
	%		{noreply, NewSD};
	%	{next, HNext, Async, NewSD} ->
	%		async(Async),
	%		NewHNext = HNext#holdem_next{
	%		             holdem_out = HO++HNext#holdem_next.holdem_out
	%		            },
	%		erlang:apply(M, F, [NewHNext | Args]),
	%		{noreply, NewSD}
	%end.
%%% ===========================================
%%%   Callbacks
%%% -------------------------------------------
handle_cast(_, SD)->
	{noreply, SD}.
code_change(_OldVersion, SD, _Extra)->
	{noreply, SD}.
terminate(_Reason, _SD)->
	ok.
%%% ===========================================
%%%   Internal
%%% -------------------------------------------
-spec reply(#done{})-> {reply, #holdem{}, sd()}.
reply(Done) ->
	{Holdem, NewSD} = timers_done(Done),
	{reply, Holdem, NewSD}.

-spec timers_done(#done{})->
		           {#holdem{}, sd()}.
timers_done(#done{sd={I,G},
                  cur=Cur,
                  cur_timer=CurTimer,
                  broadcast=BC,
                  timers=Timers})->
	Holdem=#holdem{broadcast=BC, cur=Cur},
	case Cur of
		{_Seat, _CurType}->
			%% STOPHERE: save data for snapshot:owner
			{TimerType, TimerDelay}=CurTimer,
			timers(Timers, Holdem,
			       start_waiting_timer({I,G}, TimerType, TimerDelay));
		_->
			timers(Timers, Holdem, {I,G})
	end.

%% @doc запустить таймеры или выполнить если пауза равна нулю
-spec timers([timer()], #holdem{}, sd())->
		           {#holdem{}, sd()}.
timers([{Delay, OnTimer} | NextTimers], Holdem, {I,G})
  when OnTimer=:=collect_bets ->
	if
		G#game.cur_bet=:=0->
			%% пропускаем, если не было ставок
			timers(NextTimers, Holdem, {I,G});
		Delay>0->
			%% были ставки
			start_timer(Delay, OnTimer, NextTimers),
			{Holdem, {I,G}};
		true->
			%% были ставки, Delay==0
			{NewHoldem, NewSD} = ontimer({I,G}, OnTimer),
			timers(NextTimers, extend_holdem(Holdem, NewHoldem), NewSD)
	end;
timers([{0, OnTimer} | NextTimers], Holdem, SD)->
	{NewHoldem, NewSD} = ontimer(SD, OnTimer),
	timers(NextTimers, extend_holdem(Holdem, NewHoldem), NewSD);
timers([{Delay, OnTimer} | NextTimers], Holdem, SD)->
	start_timer(Delay, OnTimer, NextTimers),
	{Holdem, SD};
timers([], Holdem, SD) ->
	{Holdem, SD}.

start_timer(Delay, OnTimer, NextTimers)->
	erlang:send_after(Delay, self(), {timer, OnTimer, NextTimers}).

-spec start_waiting_timer(sd(),
                          Type::warning | prewarinng,
                          Delay::integer())->
	sd().
start_waiting_timer({I,G}, Type, Delay)->
	Ref=erlang:start_timer(Delay, self(), waiting_timer),
	{I#info{waiting = {Type, Ref}}, G}.
stop_waiting_timer({I,G})->
	case I#info.waiting of
		{_, Ref}->
			erlang:cancel_timer(Ref),
			{I#info{waiting=false}, G};
		_ ->
			{I,G}
	end.

-spec ontimer(sd(), ontimer())->
		             {#holdem{}, sd()}.
ontimer(SD, next_blind)->
	timers_done(holdem_pregame:next_blind(SD));
ontimer(SD, preflop)->
	timers_done(holdem_next:preflop(SD));
ontimer(SD, collect_bets)->
	{NewSD, CollectBets} = holdem_batches:collect(SD),
	{#holdem{broadcast=[CollectBets]}, NewSD};
ontimer({I,G}, open_board)->
	{Level, NewCards} =
		case {G#game.opened_board_cards, G#game.board_cards} of
			{preflop, [F1,F2,F3,_T,_R]}->
				{flop, [F1,F2,F3]};
			{flop, [_F1,_F2,_F3,T,_R]}->
				{turn, [T]};
			{turn, [_F1,_F2,_F3,_T,R]}->
				{river, [R]}
		end,
	Holdem = #holdem{broadcast=[#board_cards{level=Level, cards=NewCards}]},
	{Holdem, {I, G#game{opened_board_cards=Level}}};
ontimer({I,G}, open_player_cards)->
	{Banks, BC} = open_player_cards_allin(I#info.banks),
	{DonePlayers, NewBC} = open_player_cards(G#game.done, BC),
	{#holdem{broadcast=NewBC}, {I#info{banks=Banks}, G#game{done=DonePlayers}}};
ontimer(SD, clear_moves)->
	{#holdem{broadcast=[#clear_moves{}]}, SD};
ontimer(SD, new_round_next_move)->
	timers_done(holdem_next:new_round_next_move(SD));
ontimer(SD, next_blind)->
	holdem_pregame:next_blind(SD);
ontimer(SD, OnTimer)
  when OnTimer=:=button;
       OnTimer=:=start_blinds->
	case holdem_start:try_start(SD, OnTimer) of
		{error, _Reason}->
			{#holdem{}, SD};
		Done->
			timers_done(Done)
	end;
ontimer(SD, on_switch_to_nogame)->
	case holdem_unseat:on_end_game(SD) of
		ok ->
			{#holdem{}, SD};
		{ok, Stacks, Done}->
			{#info{callback={M, F, Args}}, _} = SD,
			 erlang:apply(M, F, [{auto_unseat, Stacks} | Args]),
			timers_done(Done)
	end;
ontimer(SD, start_showdown_single_player)->
	timers_done(holdem_showdown:do_single_player(SD));
ontimer(SD, start_showdown)->
	timers_done(holdem_showdown:do(SD));
ontimer(SD, showdown_next)->
	timers_done(holdem_showdown:next(SD));
ontimer(SD, showdown_clear_losers)->
	timers_done(holdem_showdown:clear_losers(SD));
ontimer(SD, showdown_clear_winner_cards)->
	timers_done(holdem_showdown:clear_winner_cards(SD));
ontimer(SD, showdown_clear_winner_chips)->
	timers_done(holdem_showdown:clear_winner_chips(SD));
ontimer({I,G}, showdown_clear_board_cards)->
	{#holdem{broadcast=[#clear_board_cards{}]},
	 {I,G#fgame{board_cards=[]}}};
ontimer(SD, showdown_onclear)->
	timers_done(holdem_showdown:onclear(SD));
ontimer(SD, _Timer) ->
	lager:debug("unknown timer ~p", [_Timer]),
	{#holdem{}, SD}.

%%% ===========================================
%%%
%%% -------------------------------------------
-spec extend_holdem(#holdem{}, #holdem{})->
		                   #holdem{}.
extend_holdem(#holdem{cur=OldCur, broadcast=OldBC}, #holdem{cur=Cur, broadcast=BC})
  when Cur=:=unchanged->
	#holdem{cur=OldCur, broadcast=extend(OldBC, BC)};
extend_holdem(#holdem{broadcast=OldBC}, #holdem{cur=Cur, broadcast=BC}) ->
	#holdem{cur=Cur, broadcast=extend(OldBC, BC)}.

-spec extend(broadcast(), broadcast())->
		                             broadcast().
extend([], List)->
	List;
extend([One], List)->
	[One|List];
extend(List1, List2)->
	List1++List2.

open_player_cards_allin(Banks)->
	open_player_cards_allin(Banks, [], []).

open_player_cards_allin([B|Banks], BanksAcc, BC)->
	{Players, NewBC} = open_player_cards(B#bank.players, BC),
	open_player_cards_allin(Banks, [B#bank{players=Players}|BanksAcc], NewBC);
open_player_cards_allin([], BanksAcc, BC) ->
	{lists:reverse(BanksAcc), BC}.


open_player_cards(Players, BC)->
	open_player_cards(Players, [], BC).
open_player_cards([P|Players], PAcc, BC)->
	open_player_cards(Players, [P#p{is_open=true}|PAcc],
	                  [#open_player_cards{seat=P#p.seat,
	                                      card1=P#p.card1,
	                                      card2=P#p.card2}|BC]);
open_player_cards([], PAcc, BC) ->
	{PAcc, BC}.

%%% ===========================================
%%%   common tools
%%% -------------------------------------------
unixtime()->
	{Mega, Secs, _} = now(),
	Mega*1000000 + Secs.

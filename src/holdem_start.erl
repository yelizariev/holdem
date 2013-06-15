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

-module(holdem_start).
-include("holdem.hrl").
-include("holdem_internal.hrl").
-export([autoplayer/3,
         autostart/2,
         add_to_stack/3,
         seat/4,
         try_start/2]).
%%% ===========================================
%%%   Изменения, способные привести к запуску игры
%%% -------------------------------------------

%% @doc задать значение параметра и, если он ходит прямо сейчас, то
%% сделать за него ход
-spec autoplayer(sd(), seat(), AutoPlayer::boolean())
                -> #done{}.
autoplayer({I = #info{autoplayers=AP}, G},
           Seat, AutoPlayer)->
	CurSeat =
		if
			is_record(G, pregame)->
				G#pregame.cur_seat;
			is_record(G, game)->
				case G#game.wait of
					[#p{seat=S}|_]->S;
					[]->undefined
				end;
			true->
				undefined
		end,
	OldAutoPlayer = lists:member(Seat, AP),
	NewAP =
		if
			AutoPlayer=:=OldAutoPlayer->
				AP;
			AutoPlayer=:=true->
				[Seat|AP];
			AutoPlayer=:=false->
				lists:delete(Seat, AP)
		end,
	SD = {I#info{autoplayers=NewAP}, G},
	if
		Seat=/=CurSeat;
		not AutoPlayer;
		is_record(G,pregame), I#info.autoblinds->
			#done{sd=SD};
		is_record(G,pregame)->
			{ok, NotPayBlind, NewSD} = holdem_blind:not_pay(SD),
			Done = holdem_pregame:next_blind(NewSD),
			?EXTEND_BROADCAST(NotPayBlind, Done);
		is_record(G,game)->
			holdem_next:holdem_in(SD, {move, autocheck});
		is_record(G, nogame), OldAutoPlayer=:=false->
			case try_start(SD, autoplayer_on) of
				{error, _}->
					#done{sd=SD};
				Done->
					Done
			end;
		true ->
			#done{sd=SD}
	end.

-spec autostart(sd(), Autostart::boolean())->
		               #done{}.
autostart({I,G}, Autostart)->
	SD = {I#info{autostart=Autostart}, G},
	if
		Autostart=:=true, I#info.autostart=:=false->
			case try_start(SD, autostart_on) of
				{error, _} ->
					{ok, [], SD};
				Done->
					Done
			end;
		true ->
			#done{sd=SD}
	end.

-spec add_to_stack(sd(), seat(), {chips(), chips(), chips()})->
		           #done{} | {error, any()}.
add_to_stack({I, G}, Seat, {AddChips, MinStack, MaxStack})->
	case lists:keytake(Seat, #p.seat, I#info.players) of
		{value, P, Tail}->
			NewStack = P#p.stack + AddChips,
			if
				NewStack < MinStack ->
					{error, small_stack};
				NewStack > MaxStack ->
					{error, big_stack};
				true->
					Players=[P#p{stack=NewStack} | Tail],
					SD = {I#info{players=Players}, G},
					NewStackBC = {all, #new_stack{seat=Seat, stack=NewStack, add=AddChips}},
					{ok, NewStack,
					case try_start(SD, stack) of
						{error, _Reason}->
							%?DEBUG("try_start error ~p ~n~p", [_Reason, SD]),
							#done{sd=SD, broadcast=[NewStackBC]};
						Done->
							?EXTEND_BROADCAST(NewStackBC, Done)
					end
					}
			end;
		false->
			{error, player_in_game}
	end.

%% @doc просто посадить игрока или начать игру, если ждали
%% недостающего игрока. Если это первый игрок, то будет задано
%% значение #nogame.force_button
-spec seat(sd(), seat(), stack(), PlayerInfo::any())->
		          #done{}.
seat({I, G}, Seat, Stack, PlayerInfo)->
	Penalty=I#info.penalty_new_player,
	{Skipbb, NewPenalty}=
		if
			Penalty =:= false->
				{false, false};
			Penalty =:= after_next_game->
				{false, after_next_game};
			% Penalty == true
			G=:=nogame,
			length(I#info.players)<2 ->
				{false, after_next_game};
			true->
				{first_game_penalty, true}
		end,
	NewP=#p{seat=Seat,
	        stack=Stack,
	        info=PlayerInfo,
	        skipbb=Skipbb},
	
	SD = {I#info{players=[NewP|I#info.players],
	             penalty_new_player=NewPenalty},
	      if
		      I#info.players=:=[], is_record(G, nogame) ->
			      G#nogame{force_button = Seat};
		      true -> G
	      end
	     },
	NewPlayer = {all, #new_player{seat=Seat, stack=Stack, info=PlayerInfo}},
	case try_start(SD, seat) of
		{error, _}->
			#done{broadcast=[NewPlayer],
			      sd=SD};
		Done->
			?EXTEND_BROADCAST(NewPlayer, Done)
	end.

%%% ===========================================
%%%   Проверка необходимости запуска игры
%%% -------------------------------------------

%% @doc Попытаться начать игру
%%
%% * button - закончен таймер before_button*
%%
%% * start_blinds - закончен таймер before_blinds*
%%
%% * autostart_on - режима автостарта сменился на true
%%
%% * seat - подсел новый игрок
%%
%% * autoplayer_off - статус autoplayer одного игрока сменился на false
%%
%% * stack - увеличился стек одного  игрока
%%
%% * unseat -- кто-то встал из игры
-spec try_start(sd(), Reason:: button | autostart_on
                                | start_blinds
                                | unseat
                                | seat | stack | autoplayer_off
                                    )->
		               #done{}
			               | {error, not_autostart}
			               | {error, not_enough_players}
			               | {error, game_in_progress}
			               | {error, already_starting}.
try_start({_I,G}, _Reason)
  when not is_record(G, nogame)->
	{error, game_in_progress};
try_start({#info{autostart=Autostart}, _G}, _Reason)
  when Autostart =:= false->
	{error, not_autostart};
try_start({I, G}, Reason)->
	IsActive =
		case I#info.autoplayer_strategy of
			blind_check_fold->
				fun(#p{stack=S}) when S>0->true;
				   (_)->false
				end;
			noblind_check_fold->
				AP = I#info.autoplayers,
				fun(#p{stack=S}) when S>0-> not lists:member(S, AP);
				   (_)->false
				end
		end,
	{Active, NoActive} = lists:partition(IsActive, I#info.players),

	case G#nogame.timer of
		_
		  when length(Active)<2 ->
			#done{sd={I,G#nogame{timer=false}}};
			%{error, not_enough_players};

		Timer
		  when Timer=:=button, Reason=:=button;
		       Timer=:=false , Reason=:=autostart_on
		       ->
			%% вычисление и перемещение баттона, запуск before_blinds
			do_button({I,G}, Active, NoActive);

		Preblinds
		  when is_record(Preblinds, preblinds),
		       Reason=:=stack orelse
		       Reason=:=seat orelse
		       Reason=:=autoplayer_off orelse
		       Reason=:=unseat
		       ->
			%% повторное вычисление и перемещение баттона
			redo_button({I,G}, Active, NoActive);

		false
		  when Reason=:=stack;
		       Reason=:=seat;
		       Reason=:=autoplayer_off
		       ->
			%% запуск before_button_onplayers
			#done{sd={I,G#nogame{timer=button}},
			      timers=[{?TIMER(I, before_button_onplayers), button}]};
		Preblinds
		  when is_record(Preblinds, preblinds),
		       Reason=:=start_blinds ->
			%% сбор блайндов
			holdem_pregame:start(I, Preblinds);
		Timer
		  when Timer=/=false ->
			{error, already_starting}
	end.

redo_button({I,G}, Active, NoActive)->
	do_button({I,G}, Active, NoActive, true).
do_button({I,G}, Active, NoActive)->
	do_button({I,G}, Active, NoActive, false).
do_button({I,G}, Active, NoActive, IsRedo)->
	{ok, NewButton, Preblinds} =
		holdem_pregame:init(G, Active, NoActive),
	SD = {I#info{button=NewButton}, G#nogame{timer=Preblinds}},
	HOButton = #button{seat=NewButton},
	if
		IsRedo->
			#done{broadcast = [HOButton], sd = SD};
		true->
			#done{broadcast = [HOButton], sd = SD,
			      timers=[{?TIMER(I, before_blinds), start_blinds}]
			     }
	end.


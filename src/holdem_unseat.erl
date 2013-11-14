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

-module(holdem_unseat).
-include("holdem.hrl").
-include("holdem_internal.hrl").
-export([do/2, on_end_game/1, player_reconnected/2]).

-spec do(sd(), seat())->
		        {ok, {stack, stack()}, #done{}} |
		        {ok, player_in_game, #done{}}.
do(SD_, Seat)->

	Done = #done{sd = SD} = holdem_start:autoplayer(SD_, Seat, true),
	case do(SD, Seat, false) of
		{player_in_game}->
			{I,G}=SD,
			UnseatPlayers = [Seat|lists:delete(Seat,I#info.unseat_players)],
			{ok, player_in_game, Done#done{
			                       sd={I#info{unseat_players = UnseatPlayers},G}
			                      }};
		{ok, {stack, Stack}, #done{
		       broadcast=BC,
		       sd=SD1
		      }}->
			#done{sd=SD2} = holdem_start:autoplayer(SD1, Seat, false),
			{I,G} = SD2,
			UnseatPlayers = lists:delete(Seat,I#info.unseat_players),
			NewDone = Done#done{
			            sd={I#info{unseat_players=UnseatPlayers}, G},
			            broadcast = Done#done.broadcast ++ BC
			           },
			{ok, {stack, Stack}, NewDone}
	end.

-spec do(sd(), seat(), boolean())->
		        {ok, {stack, stack()}, #done{}} |
		        {player_in_game}.
do({I,G}, Seat, OnlySimpleUnseat)->
	case lists:keytake(Seat, #p.seat, I#info.players) of
		{value, P, []}
		  when is_record(G, nogame)->
			{ok, {stack, P#p.stack},
			 #done{broadcast=[#player_leave{seat=Seat},
			                  #button{seat=undefined}],
			       sd=fix_sb_bb_seat({I#info{players=[],
			                                 button=undefined,
			                                 penalty_new_player=after_next_game}, G}, Seat)}};
		{value, P, Players}
		  when is_record(G, nogame),
		       is_record(G#nogame.timer, preblinds)->
			{ok, {stack, P#p.stack}, holdem_start:try_start(fix_sb_bb_seat({I#info{players=Players}, G}, Seat), unseat)};
		{value, P, Players}->
			fix_button({ok, {stack, P#p.stack},
			            #done{sd={I#info{players=Players}, G}}},
			           Seat);
		false when not OnlySimpleUnseat->
			{player_in_game}
		%false when not OnlySimpleUnseat->
		%	fix_button(unseat_crazy({I, G}, Seat), Seat)
	end.
player_reconnected({I,G}, Seat)->
	UnseatPlayers = lists:delete(Seat, I#info.unseat_players),
	{I#info{unseat_players=UnseatPlayers}, G}.

-spec on_end_game(sd())-> {ok, [{seat(), stack()}], #done{}} | ok.
on_end_game({I,G})
  when is_record(G,nogame),
       not is_record(G#nogame.timer, preblinds)->
	on_end_game({I#info{unseat_players=[]},G}, I#info.unseat_players, [], []).
on_end_game({I,G}, [Seat|UnseatPlayers], BCAcc, Stacks)->
	Autoplayers = lists:delete(Seat, I#info.autoplayers),
	{ok, {stack, Stack}, Done} = do({I#info{autoplayers=Autoplayers},G}, Seat, true),
	#done{broadcast=BC, sd={NewI, NewG}} = Done,
	on_end_game({NewI, NewG}, UnseatPlayers, BC++BCAcc, [{Seat, Stack} | Stacks]);
on_end_game({I,G}, [], BC, Stacks)->
	{ok, Stacks, #done{broadcast=BC, sd={I,G}}}.
%%% ===========================================
%%%   internal
%%% -------------------------------------------

fix_button({ok, {stack, Stack}, Done = #done{sd={I,G}}}, Seat)->
	AllPlayers = all_players({I,G}),
	PenaltyNewPlayer=
		if
			length(AllPlayers)<2, I#info.penalty_new_player==true ->
				after_next_game;
			true->
				I#info.penalty_new_player
		end,
	{Button, BC}=
		if length(AllPlayers)=/=0->
				{undefined, []};
			I#info.button==Seat ->
				[#p{seat=Button_}|_]=holdem_sort:left(AllPlayers, Seat),
				{Button_, [#button{seat=Button_}]};
		   true->
				{I#info.button, []}
		end,
	NewG=
		case AllPlayers of
			[_] when is_record(G, nogame) ->
				G#nogame{force_button=Button};
			_-> G
		end,
	{ok, {stack, Stack}, Done#done{sd=fix_sb_bb_seat({I#info{button=Button,
	                                                penalty_new_player=PenaltyNewPlayer},NewG}, Seat),
	                      broadcast=[#player_leave{seat=Seat}|BC++Done#done.broadcast]}}.

fix_sb_bb_seat({I,G}, Seat)
  when is_record(G, nogame), G#nogame.sb_seat_old==Seat->
	{I, G#nogame{sb_seat_old=undefined}};
fix_sb_bb_seat({I,G}, Seat)
  when is_record(G, nogame), G#nogame.bb_seat_old==Seat->
	{I, G#nogame{bb_seat_old=undefined}};
fix_sb_bb_seat({I,G}, Seat)
  when is_record(G,game), I#info.sb_seat==Seat ->
	{I#info{sb_seat=undefined}, G};
fix_sb_bb_seat({I,G}, Seat)
  when is_record(G,game), I#info.bb_seat==Seat ->
	{I#info{bb_seat=undefined}, G};
fix_sb_bb_seat({I,G}, _Seat)->
	{I,G}.

-spec unseat_crazy(sd(), seat())->
		        {ok, {stack, stack()}, #done{}}.
unseat_crazy({I, Pre = #pregame{
                   sb=SBValue,
                   bb=BBValue,
                   penalty=Penalty,
                   ingame=Ingame}},
             Seat)->
	%% TODO: проверить, что достаточно игроков для продолжения (если не
	%% достаточно, то заменить #pregame на #nogame и добавить по
	%% необходимости +#blind_death)
	SBTake=keytake(SBValue, Seat),
	BBTake=keytake(SBValue, Seat),
	IngameTake=keytake(Ingame, Seat),
	PenaltyTake=keytake(Penalty, Seat),
	case {SBValue, BBValue} of
		_ when is_record(SBValue, p), SBValue#p.seat=:=Seat->
			%% игрок должен ходить сейчас
			unseat_now_move({I, Pre}, Seat, disagree_blind);
		{[P|_], _}
		  when P#p.seat=:=Seat->
			%% игрок должен ходить сейчас
			unseat_now_move({I, Pre}, Seat, disagree_blind);
		{_, [P|_]}
		  when P#p.seat=:=Seat->
			%% игрок должен ходить сейчас
			unseat_now_move({I, Pre}, Seat, disagree_blind);
		_ when SBTake=/=false->
			%% нужно удалить игрока из списка
			{value, P, List}=SBTake,
			{ok, {stack, P#p.stack}, #done{sd={I, Pre#pregame{sb=List}}}};
		_ when BBTake=/=false, length(Penalty)>0 ->
			%% нужно удалить игрока из списка
			{value, P, List}=BBTake,
			{ok, {stack, P#p.stack}, #done{sd={I, Pre#pregame{bb=List}}}};
		{_, [P]} when P#p.seat=:=Seat, length(Penalty)=:=0 ->
			%% head's up
			%% unseat игрок на ББ, но МБ еще не поставил блайнд
			%% делаем за МБ disagree_blind и спокойно убираем ББ
			unseat_now_move({I, Pre}, Seat, disagree_blind); %% ошибки нет -- disagree_blind относиться к тому, кто сейчас ходит, Seat -- кого убираем из игры
		_ when IngameTake=/=false,
		       SBValue=:=true,
		       BBValue=:=true->
			{value, P, List}=IngameTake,
			{ok, {stack, P#p.stack}, #done{sd={I, Pre#pregame{ingame=List}}}};
		_ when PenaltyTake=/=false ->
			%% просто убираем из списка.
			{value, P, List}=PenaltyTake,
			{ok, {stack, P#p.stack}, #done{sd={I, Pre#pregame{penalty=List}}}}
	end;

unseat_crazy({I = #info{
                allin=Allin,
                banks=Banks
               },
              G = #game{done=DonePlayers,
                        wait=Wait}}, Seat) ->
	IsNowMove=
		case Wait of
			[P0 | _] when P0#p.seat=:=Seat->
				true;
			_->
				false
		end,
	NewBanks = take_from_banks(Banks, Seat),
	%% NewBanks = false | [#b] (игрок удален из #bank.players)

	AllinTake=lists:keytake(Seat, #p.seat, Allin),
	WaitTake=lists:keytake(Seat, #p.seat, Wait),
	DonePlayersTake=lists:keytake(Seat, #p.seat, DonePlayers),
	%MaxBet = max_bet(Allin++DonePlayers),
	if
		IsNowMove->
			unseat_now_move({I, G}, Seat, fold);
		NewBanks=/=false->
			%% игрок в аллине с предыдущих раундов
			{ok, {stack, 0}, #done{sd={I#info{banks=NewBanks}, G}}};
		length(DonePlayers)+length(Wait)=:=2, AllinTake==false->
			%% нужно досрочное вскрытие.
			{value, P, [OtherPlayer]}=lists:keytake(Seat, #p.seat, Wait++DonePlayers++Allin),
			%% OtherPlayer не может быть в #info.allin
			{SD, Stack, OutFB, _NewCurBet} =
				fix_unseat_bet({I#info{allin=[]}, G#game{wait=[],
				                                         done=[OtherPlayer]}},
				               P),
			Done = holdem_next:premature_showdown(SD),
			{ok, {stack, Stack}, Done#done{broadcast=OutFB++Done#done.broadcast}};
		WaitTake=/=false->
			%% ждал своего хода, круг будет продолжен без него
			{value, P, NewWait} = WaitTake,
			{SD, Stack, OutFB, _NewCurBet} =
				fix_unseat_bet({I, G#game{wait=NewWait}}, P),
			{ok, {stack, Stack}, #done{broadcast=OutFB, sd=SD}};
		DonePlayersTake=/=false;
		AllinTake=/=false->
			%% игрок уже сходил. Возможно, нужно будет отправить
			%% обновленную инфу о текущем уровне ставок.
			{P, NewDonePlayers, NewAllin}=
				case {DonePlayersTake, AllinTake} of
					{{value, P_, PP}, false}->
						{P_, PP, Allin};
					{false, {value, P_, PP}}->
						{P_, DonePlayers, PP}
				end,
			{{I2, G2}, Stack, OutFB, NewCurBet} =
				fix_unseat_bet({I#info{allin=NewAllin}, G#game{done=NewDonePlayers}}, P),
			if
				NewCurBet=:=G#game.cur_bet ->
					{ok, {stack, Stack}, #done{sd={I2, G2}, broadcast=OutFB}};
				true->
					%% нужно менять cur_bet
					%%
					%% FIXME: нет возможности восстановить правильное
					%% значение delta_raise
					SD = {I2, G2#game{cur_bet=NewCurBet,
					                  delta_raise=I#info.bb}},
					Done = holdem_next:next_player_move(SD),
					{ok, {stack, Stack}, Done#done{broadcast=OutFB++Done#done.broadcast}}
			end
	end.

unseat_now_move(SD, Seat, AutoMove)->
	Done=holdem_next:holdem_in(SD, {move, AutoMove}),
	{ok, {stack, Stack}, #done{broadcast=BC, sd=NewSD}} = do(Done#done.sd, Seat, true),
	{ok, {stack, Stack}, Done#done{sd=NewSD, broadcast=Done#done.broadcast++BC}}.

%% @spec(SD, P) -> {NewSD, Stack, Out, NewCurBet}
%% Out = [#fix_batches_crazy] | []
%%
%% @doc T не содержит игрока P.
%% Изменяет #info.dead
fix_unseat_bet({I, G}, P)->
	OtherPlayers = [Bet || #p{bet=Bet} <- G#game.wait++G#game.done++I#info.allin],

	MaxBet = 
		if
			length(OtherPlayers) > 0 ->
				lists:max(OtherPlayers);
			true->
				0
		end,
	{Bet, Stack, NewCurBet, Out}=
		if
			MaxBet >= P#p.bet ->
				{#bet{seat=P#p.seat,
				      bet=P#p.bet,
				      batches=P#p.batches},
				 P#p.stack,
				 G#game.cur_bet,
				 []};
			true->
				Delta = P#p.bet-MaxBet,
				{DeltaBatches, NewBatches} =
					holdem_batches:bet(I#info.allin, MaxBet, P#p.batches, length(I#info.banks)-1),
				{#bet{seat=P#p.seat,
				      bet=MaxBet,
				      batches=NewBatches},
				 P#p.stack, % + Delta -- добавлять не будем. Delta пропадает
				 MaxBet,
				 [#fix_batches{data=#bet_data{seat=P#p.seat,
				                              batches=DeltaBatches},
				               delta=Delta}]}
			end,
	{{I#info{dead=[Bet|I#info.dead]}, G},
	 Stack, Out, NewCurBet}.

%% @spec ([#bank], Seat)-> [#bank] | false
%% Если игрок обнаружен в #bank.players, то он удаляется
take_from_banks(Banks, Seat)->
	take_from_banks(Banks, Seat, []).

%% @spec (Banks, Seat, BanksAcc)->
take_from_banks([B|Banks], Seat, BanksAcc)->
	case lists:keytake(Seat, #p.seat, B#bank.players) of
		false->
			take_from_banks(Banks, Seat, [B|BanksAcc]);
		{value, _, Players}->
			lists:reverse([B#bank{players=Players}|BanksAcc])++Banks
	end;

take_from_banks([], _Seat, _BanksAcc) ->
	false.


keytake(List, Seat) when is_list(List)->
	case lists:keytake(Seat, #p.seat, List) of
		false->
			false;
		Take -> Take
	end;
keytake(_, _) ->
	false.

all_players({I,G})->
	lists:flatten([[PP||#bank{players=PP}<-I#info.banks],
	               I#info.players,
	               case G of
		               #pregame{sb=SB,
		                        bb=BB,
		                        penalty=Penalty,
		                        ingame=Ingame}->
			               [if is_record(SB, p); is_list(SB)-> SB;
			                   true-> [] end,
			                if is_list(BB)->BB;
			                   true-> [] end,
			                if is_list(Penalty)->Penalty;
			                   true-> [] end,
			                Ingame
			               ];
		               #game{wait=Wait, done=Done}->
			               [Wait, Done];
		                #fgame{players=A1, winners=A2, losers=A3}->
			               [A1, A2, A3];
		               _ -> []
	               end
	              ]).

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

-module(holdem_batches).
-include("holdem.hrl").
-include("holdem_internal.hrl").
-export([fix/1, collect/1, bet/4, collect_ante_batches/2]).


-type allin_num()::false|integer().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% ===========================================
%%%   collect_ante_batches
%%% -------------------------------------------

%% @doc создает банки, перемещает в них аллин игроков. Не изменяет
%% стеки не аллин игроков
%% Seats = [seat()] - список не алл-ин игроков
-spec collect_ante_batches(sd(), [seat()]) ->
		      {sd(), ante_value()}.
collect_ante_batches({I = #info{allin=Allin0,
                               ante=Ante},
                      Pre},
                     Seats)->
	Allin = [P#p{stack=0, bet=P#p.stack, batches=[P#p.stack]} || P <- Allin0],
	AllinLevels=lists:usort([B || #p{bet=B} <- Allin]),
	{NewBanks, AllinBets} =
		new_banks(AllinLevels, [#bank{}], Allin, []),
	{_, AllinBets2} = fix_bets(AllinBets, AllinLevels, 0),


	NotAllinBets = [#bet{seat=S, bet=Ante, batches=[Ante]}
	               || S <- Seats],
	NotAllinLevels=lists:usort([Ante|AllinLevels]),
	{_, NotAllinBets2} = fix_bets(NotAllinBets, NotAllinLevels, false),


	{FilledBanks, _} = fill_banks(NewBanks, AllinBets2++NotAllinBets2, false),
	AnteValue =
		[#bet_data{seat=Seat,
		           batches=Batches,
		           allin=length(Batches)-1}
		 || #bet{seat=Seat, batches=Batches} <- AllinBets2]
		++
		[#bet_data{seat=Seat,
		           batches=Batches}
		 || #bet{seat=Seat, batches=Batches} <- NotAllinBets2],

	{{I#info{banks=FilledBanks, allin=[]}, Pre}, AnteValue}.
%%% ===========================================
%%%   collect batches
%%% -------------------------------------------
%% @doc подправляет #bet.batches в списках #game.done #info.allin
%% #info.dead
-spec fix(sd())->
		         {sd(),
		          [#fix_batches{}],
		          integer() % количество банков данного раунда
		         }.
fix({I = #info{allin=Allin, dead=Dead, banks=Banks},
     G = #game{cur_bet=CurBet, done=Done}})->
	AllinLevels=lists:usort([B || #p{bet=B} <- Allin]),
	LevelsCurAndAllin=lists:usort([CurBet|AllinLevels]),
	Levels=lists:duplicate(length(Banks)-1, 0)++LevelsCurAndAllin,
	{FB1, NewDead} = fix_bets(Dead, Levels, false),
	{FB2, NewDone} = fix_player_batches(Done, Levels, false, FB1, []),
	{FB3, NewAllin} = fix_player_batches(Allin, Levels, length(Banks)-1, FB2, []),
	SD = {I#info{allin=NewAllin, dead=NewDead}, G#game{done=NewDone}},
	{SD, FB3, length(LevelsCurAndAllin)}.

%% OLD: collect_batches
%% @spec(T) -> {T, FixBatches, Rakes}
%% FixBatches = #fix_batches.players
%% @doc
%% OLD Использует #game.cur_bet
%% OLD: Очищает: #info.allin, #info.dead
%% OLD: Изменяет: #game.done, #info.banks

%%% @doc Собрать фишки игроков в банк. Предполагает, что перед этим сделан вызов fix/1
-spec collect(sd())->
		             {sd(), #collect_bets{}}.
collect({I = #info{allin=Allin,
                   rake=RakeFactor,
                   dead=Dead,
                   banks=[CurBank|OldBanks]
                  },
         G = #game{done=Done}})->
	AllinLevels=lists:usort([B || #p{bet=B} <- Allin]),
	{NewBanks, AllinBets} = new_banks(AllinLevels, [CurBank], Allin, Dead),
	{NewDone, Bets} = take_bets(Done, AllinBets, []),
	[LastBank|Tail]=NewBanks,
	NewBanks2=
		if
			RakeFactor=:=false;
			length(Done)+length(LastBank#bank.players)>1 ->
				NewBanks;
			true->
				%% банк принадлежит только одному игроку -- рейк не взимается.
				[LastBank#bank{is_disabled_rake=true}|Tail]
		end,
	{FilledBanks, Rakes} = fill_banks(NewBanks2++OldBanks, Bets, RakeFactor),
	{{I#info{banks=FilledBanks,
	         dead=[],
	         allin=[]},
	 G#game{done=NewDone}},
	 #collect_bets{rakes=Rakes}}.

%% @spec ()-> {FilledBanks, Rakes}
%% @doc изменяет только #bank.chips
fill_banks(Banks, Bets, RakeInfo)->
	fill_banks(lists:reverse(Banks), Bets, [], RakeInfo, []).
fill_banks([B|Banks], Bets, BanksAcc, RakeFactor, Rakes) ->
	{AddToBank, NewBets} = fill_bank(Bets, [], 0),
	{NewRakes, NewB}=
		if
			RakeFactor=:=false->
				{[], B#bank{chips=B#bank.chips+AddToBank}};
			B#bank.is_disabled_rake ->
				{[0|Rakes], B#bank{chips=B#bank.chips+AddToBank}};
			true->
				R = ?RAKE(AddToBank, RakeFactor),
				{[R|Rakes], B#bank{chips=B#bank.chips+AddToBank-R}}
	end,
	fill_banks(Banks, NewBets, [NewB|BanksAcc], RakeFactor, NewRakes);
fill_banks([], _Bets, BanksAcc, _RakeInfo, Rakes) ->
	{BanksAcc, lists:reverse(Rakes)}.

%% @spec (Bets, BetsAcc, ChipsAcc) -> {AddToBank, NewBets}
fill_bank([B = #bet{batches=[Chips|Tail]}|Bets],
          BetsAcc, ChipsAcc)->
	fill_bank(Bets, [B#bet{batches=Tail} | BetsAcc], ChipsAcc+Chips);
fill_bank([_B|Bets], BetsAcc, ChipsAcc) ->
	%% empty #bet
	fill_bank(Bets, BetsAcc, ChipsAcc);
fill_bank([], BetsAcc, ChipsAcc) ->
	{ChipsAcc, BetsAcc}.


%% @spec(Levels, Banks, Allin::[#p], Bets) ->
%% {NewBanks, AllinBets}
%% @doc распределяет игроков из #info.allin по горшкам, очищает их
%% ставки (#p.bet, #p.batches).
new_banks(_Levels, Banks, [], Bets)->
	{Banks, Bets};
new_banks([L|Levels], [Bank|Banks], Allin, Bets)->
	{NewBank, NewAllin, NewBets} =
		move_to_banks(Allin, Bank, L, Bets, []),
	NewBanks = [#bank{}, NewBank | Banks],
	new_banks(Levels, NewBanks, NewAllin, NewBets).





%% @spec (Allin, Bank, Level, Bets, AllinAcc)-> {NewBank, NewAllin, NewBets}
%% NewAllin, AllinAcc - те, которые не попали в NewBank
%% @doc перемещает игроков со ставкой==Level в Bank. Ставка
%% перемещается в #bet
move_to_banks([P|Allin], Bank, Level, Bets, AllinAcc)
  when P#p.bet=:=Level ->
	move_to_banks(Allin, Bank#bank{players=[P#p{bet=0, batches=[0]}
	                                        |Bank#bank.players]},
	              Level, [#bet{seat=P#p.seat,
	                           bet=P#p.bet,
	                           batches=P#p.batches}
	                      | Bets],
	             AllinAcc);
move_to_banks([P|Allin], Bank, Level, Bets, AllinAcc) ->
	move_to_banks(Allin, Bank, Level, Bets, [P|AllinAcc]);
move_to_banks([], NewBank, _Level, NewBets, NewAllin) ->
	{NewBank, NewAllin, NewBets}.

%% @spec (Players, AccBets, AccPlayers)-> {Players, Bets}
%% @doc очищает #p.bet, #p.batches в списках #info.allin,
%% #game.done. Очищает список #info.dead
take_bets([P = #p{seat=Seat, bet=Bet, batches=Batches}|Players],
           AccBets, AccPlayers)->
	take_bets(Players,
	          [#bet{seat=Seat, bet=Bet, batches=Batches}|AccBets],
	          [P#p{bet=0, batches=[0]}|AccPlayers]);
take_bets([], AccBets, AccPlayers) ->
	{AccPlayers, AccBets}.

-spec fix_bets([#bet{}], Levels::[integer()], allin_num())
               -> {[#fix_batches{}], [#bet{}]}.
fix_bets(Bets, [_Level], _AllinNum)->
	{[], Bets};
fix_bets(Bets, Levels, AllinNum)->
	fix_bets(Bets, Levels, AllinNum, [], []).
%% @spec (Bets, Levels, FBAcc, BetsAcc)
fix_bets([B = #bet{batches=OldBatches}|Bets],
         Levels, AllinNum, FBAcc, BetsAcc)->
	case check_batches(Levels, AllinNum, B#bet.seat, B#bet.bet, OldBatches) of
		ok->
			fix_bets(Bets, Levels, AllinNum, FBAcc, [B|BetsAcc]);
		{fix, FB, NewBatches}->
			fix_bets(Bets, Levels, AllinNum,
			         [FB | FBAcc],
			         [B#bet{batches=NewBatches}|BetsAcc])
	end;
fix_bets([], _Levels, _AllinNum, FBAcc, BetsAcc) ->
	{FBAcc, BetsAcc}.


-spec fix_player_batches([#p{}],
                         Levels::[integer()],
                         AllinNum::false|integer(),
                         FBAcc::[#fix_batches{}],
                         PAcc :: [#p{}])
                        -> {[#fix_batches{}], [#p{}]}.
fix_player_batches([P=#p{seat=Seat, bet=Bet, batches=Batches}|Players],
                   Levels, AllinNum, FBAcc, PAcc)->
	case check_batches(Levels, AllinNum, Seat, Bet, Batches) of
		ok ->
			fix_player_batches(Players, Levels, AllinNum, FBAcc, [P|PAcc]);
		{fix, FB, NewBatches} ->
			fix_player_batches(Players, Levels, AllinNum,
			                   [FB|FBAcc],
			                   [P#p{batches=NewBatches}|PAcc])
	end;
fix_player_batches([], _, _, FBAcc, PAcc) ->
	{FBAcc, PAcc}.


check_batches(Levels, AllinNum, Seat, BetValue, OldBatches)->
	case batches(Levels, BetValue, OldBatches, 0, [], []) of
		{_DeltaBatches, OldBatches}->
			ok;
		{DeltaBatches, NewBatches} ->
			Allin =
				case AllinNum of
					false->false;
					_-> length(NewBatches)-1+AllinNum
				end,
			{fix, #fix_batches{data=#bet_data{seat=Seat,
			                                  batches=DeltaBatches,
			                                  allin=Allin}},
			 NewBatches}
	end.


%%% ===========================================
%%%   Bet
%%% -------------------------------------------

%% OLD: batches
%% @spec(Allin::[#p], PBet, OldBatches, ZeroLen)-> {DeltaBatches, NewBatches}
%% PBet - ставка, которую нужно раскидать по кучкам.
%% ZeroLen -- сколько должно быть первых нулей ( = номеру текущего банка)
bet([], PBet, [OldBet], 0) ->
	%% наиболее типичный случай -- нет побочных банков, нет аллинщиков
	{[PBet-OldBet], [PBet]};
bet(Allin, PBet, OldBatches, ZeroLen) ->
	Levels=lists:duplicate(ZeroLen, 0)++lists:usort([PBet | [B || #p{bet=B} <- Allin]]),
	batches(Levels, PBet, OldBatches, 0, [], []).

-ifdef(EUNIT).
bet_test_()->
	ALLIN1= [#p{seat=2, bet=300, batches=[300]}],
	ALLIN2 = [#p{seat=2, bet=300, batches=[100, 200]},
                 #p{seat=5, bet=200, batches=[200]}
                ],
	[
	?_assertEqual({[200], [500]},
	              holdem_batches:bet([], 500, [300], 0)),
	?_assertEqual({[200, 700], [300, 700]},
	              holdem_batches:bet(ALLIN1, 1000, [100], 0)),
	?_assertEqual({[0, 0, 200, 700], [0, 0, 300, 700]},
	              holdem_batches:bet(ALLIN1, 1000, [0, 0, 100], 2)),
	?_assertEqual({[0, 50, 700], [200, 100, 700]},
	              holdem_batches:bet(ALLIN2, 1000, [200, 50], 0)),
	?_assertEqual({[0, 0, 50, 700], [0, 200, 100, 700]},
	              holdem_batches:bet(ALLIN2, 1000, [0, 200, 50], 1)),
	?_assertEqual({[0, 1000], [0, 1000]},
	              holdem_batches:bet([], 1000, [0], 1))
	].
-endif.

%% @spec (Levels, Chips, OldBatches, PrevLevel, DeltaAcc, NewAcc)
%%
%% Chips - оставшиеся фишки, которые раскидываем
%%
%% Levels - [Bet0, Bet1, ...] - ставки алл-ин игроков. Сортировка:
%% наименьшая ставка слева
%%
%% NewAcc - acc for NewBatches
batches(_, 0, _, _, DeltaAcc, NewAcc) ->
	{lists:reverse(DeltaAcc), lists:reverse(NewAcc)};
batches([L|Levels], Chips, OldBatches, PrevLevel, DeltaAcc, NewAcc)->
	[Old|OldBatches2]=
		case OldBatches of
			[]->
				%% раньше было меньше кучек
				[0];
			_->
				OldBatches
		end,
	MaxCur=L-PrevLevel,
	Cur=
		if
			MaxCur>Chips -> Chips;
			true->MaxCur
		end,
	batches(Levels, Chips-Cur, OldBatches2, L,
	        [Cur-Old|DeltaAcc],
	        [Cur|NewAcc]).

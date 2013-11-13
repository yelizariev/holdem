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

-module(holdem_snapshot).
-include("holdem.hrl").
-include("holdem_internal.hrl").
-export([do/2, get_stack/2]).

-spec do(sd(), Owner::false|seat())->
		        {ok, #holdem_snapshot{}, broadcast()}.
-define(HSP, holdem_snapshot_player).

get_stack({I,G}, Seat)->
	{Players, _Bets} = players_and_bets({I,G}),
	case lists:keyfind(Seat, #?HSP.seat, Players) of
		#?HSP{stack=Stack, cards=Cards}->
			{ok, Stack, Cards/=false};
		false ->
			{error, not_found}
	end.

do({I=#info{sb=SBChips,
            bb=BBChips,
            ante=Ante,
            button=Button,
            banks=BankList,
            max_players=MaxPlayers
           }, G}, Owner)->
	BoardCards=
		case G of
			#game{opened_board_cards=flop,
			      board_cards= [Flop1, Flop2, Flop3, _Turn, _River]}->
				[Flop1, Flop2, Flop3];
			#game{opened_board_cards=turn,
			      board_cards= [Flop1, Flop2, Flop3, Turn, _River]}->
				[Flop1, Flop2, Flop3, Turn];
			#game{opened_board_cards=river,
			      board_cards= [Flop1, Flop2, Flop3, Turn, River]}->
				[Flop1, Flop2, Flop3, Turn, River];
			#fgame{board_cards=Cards}->
				Cards;
			_ ->
				[]
		end,
	Banks = [Chips || #bank{chips=Chips} <- BankList],
	WinnerChips=
		case G of
			#fgame{winners=Winners, losers=Losers}->
				lists:foldl(fun(#p{seat=Seat, bet=Chips}, Acc)
					              when Chips>0->
						            [{Seat, Chips}|Acc];
				               (_, Acc)->Acc
				            end, [], Winners++Losers);
			_->
				[]
		end,

	{Players, Bets} = players_and_bets({I,G}),
	Highlight=
		case G of
			#fgame{cur_highlight=CurHighlight} ->
				CurHighlight;
			_->
				false
		end,
	{OwnerValue, BC}=owner({I,G}, Owner),
	PlayersSnapshot = [P#?HSP{autoplayer=lists:member(P#?HSP.seat, I#info.autoplayers)} || P <- Players],
	Snapshot=
		#holdem_snapshot{sb=SBChips, bb=BBChips, ante=Ante,

		                 banks=Banks,
		                 bets=Bets,

		                 board=BoardCards,
		                 button=Button,
		                 highlight=Highlight,
		                 max_players=MaxPlayers,
		                 owner=OwnerValue,
		                 winner_chips=WinnerChips,
		                 players=PlayersSnapshot
		                },
	{ok, Snapshot, BC}.

players_and_bets({#info{
                    sb=SBChips,
                    bb=BBChips,
                    banks=BankList,
                    players=InfoPlayers,
                    dead=Dead,
                    allin=InfoAllin
                    },G})->
	BankNum=length(BankList)-1,
	%% Bets and Players
	%% dead bets
	Bets1 = [{Seat, Batches} || #bet{seat=Seat, batches=Batches} <- Dead],
	%% not in game players
	Players1= not_in_game_players(InfoPlayers, []),
	%% alin players
	Players2 = bank_players(BankNum, BankList, Players1),
	%% cur allin players, bets
	{Players_, Bets_} = cur_allin_players(BankNum, InfoAllin, Players2, Bets1),
	%% in game players, bets
	case G of
		_ when is_record(G, nogame)->
			%% FIXME: #nogame
			{Players_, Bets_};
		#pregame{sb=SB,
		         bb=BB,
		         penalty=Penalty,
		         ingame=Ingame} ->
			NotInGame=
				if
					is_record(SB, p)-> [SB];
					is_list(SB) -> SB;
					true -> []
				end ++
				if
					is_list(BB) -> BB;
					true -> []
				end ++ Penalty,
			PlayersTmp=not_in_game_players(NotInGame, Players_),
			blind_players(SBChips, BBChips, Ingame, PlayersTmp, Bets_);
		#game{done=Done, wait=Wait} ->
			game_players(Done++Wait, Players_, Bets_);
		#fgame{winners=Winners2, losers=Losers2, players=FGamePlayers} ->
			game_players(Winners2++Losers2++FGamePlayers, Players_, Bets_)
	end.


not_in_game_players([P|Players], PAcc)->
	#p{seat=Seat, stack=Stack, info=Info, last_action=Action}=P,
	Move=
		case Action of
			fold->fold;
			_->false
		end,
	HSP=#?HSP{seat=Seat, info=Info, move=Move,
	           cards=false, stack=Stack},
	not_in_game_players(Players, [HSP|PAcc]);
not_in_game_players([], PAcc) ->
	PAcc.


bank_players(BankNum, [B|Banks], PAcc)->
	bank_players(BankNum-1, Banks, allin_players(BankNum, B#bank.players, PAcc));
bank_players(-1, [], PAcc)->
	PAcc.

allin_players(BankNum, [P|Players], PAcc)->
	#p{seat=Seat,
	   info=Info,
	   stack=0
	  }=P,
	HSP=#?HSP{seat=Seat, info=Info, stack=0, cards=p_to_cards(P), move={allin, BankNum}},
	allin_players(BankNum, Players, [HSP|PAcc]);
allin_players(_, [], PAcc) ->
	PAcc.

p_to_cards(#p{
	   card1=C1,
	   card2=C2,
	   is_open=IsOpenedCards}) when IsOpenedCards->
	{C1, C2};
p_to_cards(_)->
	true.

cur_allin_players(BankNum, [P|Players], PAcc, BetsAcc)->
	#p{seat=Seat,
	   info=Info,
	   stack=0,
	   batches=Batches
	  }=P,
	HSP=#?HSP{seat=Seat, info=Info, stack=0, cards=p_to_cards(P),
	           move={allin, BankNum+length(Batches)-1}},
	cur_allin_players(BankNum, Players, [HSP|PAcc], [{Seat, Batches}|BetsAcc]);
cur_allin_players(_, [], PAcc, BetsAcc)->
	{PAcc, BetsAcc}.

blind_players(SBChips, BBChips, [P|Players], PAcc, BetsAcc)->
	#p{seat=Seat,
	   info=Info,
	   stack=Stack,
	   bet=Bet,
	   batches=Batches
	  }=P,
	Move=
		if Bet=:=SBChips->sb;
		   Bet=:=BBChips->bb;
		   true->sbb
		end,
	HSP=#?HSP{seat=Seat, info=Info, stack=Stack, cards=false,
	           move=Move},
	blind_players(SBChips, BBChips, Players,
	              [HSP|PAcc],
	              [{Seat, Batches}|BetsAcc]);
blind_players(_, _, [], PAcc, BetsAcc) ->
	{PAcc, BetsAcc}.

game_players([P|Players], PAcc, BetsAcc)->
	#p{seat=Seat,
	   info=Info,
	   stack=Stack,
	   last_action=Action,
	   comb=Comb,
	   bet=Bet,
	   batches=Batches
	  }=P,
	Move=
		case Action of
			comb->Comb;
			weaker_comb->{weaker, Comb};
			_->Action
		end,
	HSP=#?HSP{seat=Seat, info=Info, stack=Stack,
	          cards=p_to_cards(P),
	          move=Move},
	NewBetsAcc=
		if
			Bet=:=0->BetsAcc;
			true->[{Seat, Batches}|BetsAcc]
		end,
	game_players(Players, [HSP|PAcc], NewBetsAcc);
game_players([], PAcc, BetsAcc) ->
	{PAcc, BetsAcc}.

owner(_, false)->
	{false, []};
owner({I,G}, Seat)
  when is_record(G, game)->
	OwnerValue=owner_cards(Seat, G#game.wait++G#game.wait++I#info.allin++lists:flatten([Players||#bank{players=Players}<-I#info.banks])),
	BC=
		case G#game.wait  of
			[#p{seat=PSeat}|_] when PSeat=:=Seat->
				NowMove=
					case G#game.cur_timer of
						{Unixtime, {warning, MSeconds}}->
							[#now_move{seat=Seat, mseconds=MSeconds - (holdem:unixtime()-Unixtime)*1000}];
						_->
							[]
					end,
				[element(2, G#game.cur_you_move)|NowMove];
			_->
				[]
		end,
	{OwnerValue, BC};
owner({I,G}, Seat)
  when is_record(G, fgame)->
	OwnerValue=owner_cards(Seat, G#fgame.winners++G#fgame.losers++G#fgame.players++lists:flatten([Players||#bank{players=Players}<-I#info.banks])),
	{OwnerValue, []};
owner({_I, G}, Seat)->
	BC=
		if
			is_record(G, pregame)->
				NowMove=
					case G#pregame.cur_timer of
						{Unixtime, {warning, MSeconds}}->
							[#now_move{seat=Seat, mseconds=MSeconds - (holdem:unixtime()-Unixtime)*1000}];
						_->
							[]
					end,
				[G#pregame.cur_req_blind|NowMove];
			true->
				[]
		end,
	{{Seat, no_cards}, BC}.


owner_cards(Seat, Players)->
	case lists:keyfind(Seat, #p.seat, Players) of
		#p{card1=C1, card2=C2}
		  when C1=/=false->
			{Seat, C1, C2};
		_->
			{Seat, no_cards}
	end.

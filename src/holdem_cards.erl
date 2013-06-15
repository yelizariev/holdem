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

%%% @doc Модуль позволяет опеределить силу карт игрока
-module(holdem_cards).
-export([
         shuffle/1,
         force/1,
         force_to_comb/3,
         best_ranks/2
        ]).

-include("holdem.hrl").
-include("holdem_internal.hrl").
-define(FLOP1, 1).
-define(FLOP2, 2).
-define(FLOP3, 4).
-define(TURN, 8).
-define(RIVER, 16).
-define(CARD1, 32).
-define(CARD2, 64).

-define(ROYAL,     9). % Флеш-рояль      
-define(SFLUSH,    8). % Стрит-флеш      
-define(QUADS,     7). % Каре            
-define(FULL,      6). % Фулхаус         
-define(FLUSH,     5). % Флеш            
-define(STRAIGHT,  4). % Стрит           
-define(SET,       3). % три одного типа 
-define(TWOP,      2). % две пары        
-define(ONEP,      1). % пара            
-define(HCARD,     0). % [старшая карта] 

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
%%% ===========================================
%%%   API
%%% -------------------------------------------
%% @doc Выдача случайного списка карт, длины N
%% @spec(N)-> [Card1, Card2, ..., CardN]
%% N <= 52
%% CardX = 0..51
shuffle(N)->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	shuffle(lists:seq(0, 51), N, []).

shuffle(_, 0, Acc)->
	Acc;
shuffle(List, N, Acc) ->
    {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
    shuffle(Leading ++ T, N-1, [H | Acc]).

%% @doc определяет силу руки
%%
%% @spec (Cards)-> #force
%% Cards = [Card1, Card2, Flop1, Flop2, Flop3, Turn, River]
force(Cards)->
	RecordCards = lists:zipwith(fun(Type, Value)->
			                            #ecard{type=Type,
			                                  rank = (Value rem 13) + 2,
			                                  suit = Value div 13}
	                            end,
	                            [?CARD1, ?CARD2,
	                             ?FLOP1, ?FLOP2, ?FLOP3, ?TURN, ?RIVER],
	                            Cards),
	SortCards=
		lists:sort(fun(#ecard{rank=R1}, #ecard{rank=R2})->
				           R1>=R2
		           end, RecordCards), % тузы в начале списка.
	flush_royal_or_less(SortCards).

-ifdef(EUNIT).
force_test_()->
	[fun()->
			 [C1, C2, C3, C4, C5, C6, C7] = [S*13+R-2 || {R, S}<-TestCards],
			 #force{type=FType, ecards=ForceCards}
				 = holdem_cards:force([C1, C2, C3, C4, C5, C6, C7]),
			 ForceCardsValue = [S*13+R-2
			                    || #ecard{rank=R, suit=S}<-ForceCards],
			 ?assertEqual({Type, [C1, C2, C3, C4, C5]}, {FType, ForceCardsValue})
	 end
	 ||
		{Type,TestCards} <-
			[
			 {?ROYAL,    [{14,0}, {13,0}, {12,0}, {11,0}, {10,0}, {2,1}, {3,1}]},
			 {?SFLUSH,   [{13,0}, {12,0}, {11,0}, {10,0}, {9,0}, {2,1}, {3,1}]},
			 {?SFLUSH,   [{5,0}, {4,0}, {3,0}, {2,0}, {14,0}, {2,1}, {3,1}]},
			 {?QUADS,    [{10,0}, {10,1}, {10,2}, {10,3}, {14,0}, {2,1}, {3,1}]},
			 {?FULL,     [{10,0}, {10,1}, {10,2}, {11,3}, {11,0}, {2,1}, {3,1}]},
			 {?FULL,     [{10,0}, {10,1}, {10,2}, {9,3}, {9,0}, {2,1}, {3,1}]},
			 {?FLUSH,    [{11,0}, {10,0}, {9,0},  {4,0}, {2,0}, {2,1}, {3,1}]},
			 {?STRAIGHT, [{13,1}, {12,0}, {11,0}, {10,0}, {9,0}, {2,1}, {3,1}]},
			 {?STRAIGHT, [{5,1}, {4,0}, {3,0}, {2,0}, {14,0}, {2,1}, {3,1}]},
			 {?FLUSH,    [{11,0}, {10,0}, {6,0},  {5,0}, {4,0}, {3,1}, {2,1}]}, %% {straight and flush, but not straight flush
			 {?FLUSH,    [{11,0}, {10,0}, {5,0},  {4,0}, {3,0}, {2,1}, {14,1}]},
			 {?SET,    [{10,0}, {10,1}, {10,2}, {14,3}, {11,0}, {2,1}, {3,1}]},
			 {?SET,    [{10,0}, {10,1}, {10,2}, {14,3}, {7,0}, {2,1}, {3,1}]},
			 {?TWOP,     [{10,0}, {10,1}, {9,2}, {9,3}, {13,2}, {2,1}, {3,1}]},
			 {?TWOP,     [{10,0}, {10,1}, {9,2}, {9,3}, {8,2}, {2,1}, {3,1}]},
			 {?ONEP,    [{10,0}, {10,1}, {9,3}, {8,0}, {7,1}, {4,1}, {2,1}]},
			 {?HCARD,    [{14,0}, {10,1}, {9,3}, {8,0}, {7,1}, {4,1}, {2,1}]}
			]
	].
-endif.


force_to_comb(#force{type=Type,
                     ecards=Cards},
              CountUsedCards_,
              IsHighligh)->
	CountUsedCards =
		if
			Type=:=0, CountUsedCards_=:=0->
				1; % для кобминации "Старшая карта" используется только название с хотя бы одной картой (т.е. вместо "Старшая карта" пишется "Туз").
			true->
				CountUsedCards_
		end,
	CombCount =
		case Type of
			9 -> 5;
			8 -> 5;
			7 -> 4;
			6 -> 5;
			5 -> 5;
			4 -> 5;
			3 -> 3;
			2 -> 4;
			1 -> 2;
			0 -> 1
		end,
	Highlight=
		if
			IsHighligh->
				HighlightCards = lists:sublist(Cards, erlang:max(CombCount, CountUsedCards)),
				lists:foldl(fun(#ecard{type=T}, HL)->
						            HL+T
				            end, 0, HighlightCards);
			true->0
		end,

	NameCards = lists:sublist(Cards, CountUsedCards),
	Ranks = u([R || #ecard{rank=R} <- NameCards]),
	#comb{type=Type, ranks=Ranks, highlight=Highlight}.

u([])->
	[];
u([E|Elements])->
	u(Elements, [E]).
u([E|Elements], [Cur|Acc])
  when E=:=Cur->
	u(Elements, [Cur|Acc]);
u([E|Elements],  Acc)->
	u(Elements, [E|Acc]);
u([], Acc) ->
	lists:reverse(Acc).

best_ranks(Cards1, Cards2)->
	best_ranks(Cards1, Cards2, 0).
best_ranks([#ecard{rank=R1}|_], [#ecard{rank=R2}|_], Used)
  when R1 > R2 -> {first, Used+1};
best_ranks([#ecard{rank=R1}|_], [#ecard{rank=R2}|_], Used)
  when R1 < R2 -> {second, Used+1};
best_ranks([_|Cards1], [_|Cards2], Used)->
	best_ranks(Cards1, Cards2, Used+1);
best_ranks([], [], 5)->
	{equal, 5}.
%%% ===========================================
%%%   Определение силы руки - straight
%%% -------------------------------------------
%% @spec(Cards7) -> no | {yes, IsFlush, Cards5}
%% IsFlush = no_flush | flush
maybe_straight(Cards7)->
	CardsAce=copy_aces_to_end(Cards7),
	CardsStraight=get_straight_cards(CardsAce),
	case CardsStraight of
		[] -> no;
		_->
			case get_flush_straight(CardsStraight) of
				no->
					{yes, no_flush, fixAces(get_first_straight(CardsStraight))};
				{yes, Cards5}->
					{yes, flush, fixAces(Cards5)}
			end
	end.
%% @doc копируем тузы в конец с заменой rank на 1
copy_aces_to_end(Cards7)->
	copy_aces_to_end(Cards7, Cards7).

copy_aces_to_end([Ace |Cards], CardsNew) when Ace#ecard.rank=:=14 ->
    copy_aces_to_end(Cards, CardsNew++[Ace#ecard{rank=1}]);
copy_aces_to_end(_, CardsNew)->
    CardsNew.

%% @doc заменяем ранг 1 на 14
fixAces(Cards5)->
	[if
		 C#ecard.rank=:=1 ->
			 C#ecard{rank=14};
		 true->
			 C
	 end || C <- Cards5].
%% @doc Оставляет те карты, которые образуют стрит.
%%
%% АЛГОРИТМ. Перебирает карты пока не найдёт карту, ранк которой
%% отличается от предыдущий больше чем на 1, либо когда закончатся карты.
%%
%% Если до этой карты карт больше 5 и
%% (РанкПервойКарты-РанкПоследней)>=4, то продолжаем перебирать. Иначе
%% карты "до" удаляются.
%%
%% Примеры: 10 8 7 7 6 5 4 -> 8 7 7 6 5 4 1
%% 1 8 7 6 5 3 2 -> []
%%
%% @spec (Cards) -> CardsStraight
get_straight_cards([Card | CardsTail])->
	get_straight_cards(CardsTail, [Card]).
%% @spec get_straight_cards(Cards, CardsNew) -> CardsStraight
%%
%% CardsNew -- отсортирован в обратном порядке (самая сильная карта справа)
%% Пример: Cards=[9, 8, 7, 7, 6] CardsNew=[10, 11]
get_straight_cards([Card|CardsTail],
                   CardsNew = [CardNew|_])
  when (CardNew#ecard.rank - Card#ecard.rank) > 1 ->
	%% случай когда обрывается цепочка
	case length(CardsNew) < 5 of
		true->
			get_straight_cards(CardsTail, [Card]);
		false->
			get_straight_cards([], CardsNew)
	end;
get_straight_cards([Card | CardsTail], CardsNew)->
	get_straight_cards(CardsTail, [Card | CardsNew]);
get_straight_cards([], CardsNew)
  when length(CardsNew) < 5 -> [];
get_straight_cards([], CardsNew)->
	%% CardsNew отсортированны в обратном порядке!
	[LastCard|_] = CardsNew,
	FirstCard = lists:last(CardsNew),
	case (FirstCard#ecard.rank - LastCard#ecard.rank) >= 4 of
		true->
			lists:reverse(CardsNew);
		false ->
			[]
	end.

%% @spec get_flush_straight(CardsStraight)-> no | {yes, Cards5}
%%
%% @doc проверяет нет ли флеш-стрита. Предполагается, что картах какой-то
%% стрит всё равно есть
get_flush_straight(CardsStraight)->
	AllFlush=[[C || C <- CardsStraight, C#ecard.suit=:=Suit]
	          || Suit <- [0, 1, 2, 3]],
	Flush=[F || F <- AllFlush, length(F) >= 5],
	case Flush of
		[]->
			no;
		[CardsFlush | _]->
			case get_straight_cards(CardsFlush) of
				[] -> no;
				[C1, C2, C3, C4, C5 | _]->
					%% среди карт нет дубликатов, т.к. все карты одной
					%% масти
					{yes, [C1, C2, C3, C4, C5]}
			end
	end.

%% @spec (CardsStraight) -> Cards5
%% @doc Убирает дубликаты (например 10 9 8 7 7 6) и хвост.
get_first_straight(CardsStraight) ->
	get_first_straight(CardsStraight, []).
%% @spec (CardsStraight, Collect5Card)-> Cards5
%%
%% Collect5Card — "собираем 5 карт"
get_first_straight(_, Collect5Card)
  when length(Collect5Card)=:=5 ->
	lists:reverse(Collect5Card);
get_first_straight([Card1, Card2, Card3 | CardsTail], Collect5Card)
  when (Card1#ecard.rank=:=Card2#ecard.rank) and (Card1#ecard.rank=:=Card3#ecard.rank)->
	get_first_straight(CardsTail, [Card1|Collect5Card]);
get_first_straight([Card1, Card2 | CardsTail], Collect5Card) when
	  (Card1#ecard.rank=:=Card2#ecard.rank) ->
	get_first_straight(CardsTail, [Card1|Collect5Card]);
get_first_straight([Card1 | CardsTail], Collect5Card)->
	get_first_straight(CardsTail, [Card1|Collect5Card]).
%%% ===========================================
%%%   Определение силы руки - флеш, ранги
%%% -------------------------------------------
%% @spec (Cards7) -> no | {yes, Cards5}
maybe_flush(Cards7)->
	AllFlush=[[C || C <- Cards7, C#ecard.suit=:=Suit]
	          || Suit <- [0, 1, 2, 3]],
	Flush=[F || F <- AllFlush, length(F) >= 5],
	case Flush of
		[]->
			no;
		[[C1, C2, C3, C4, C5 | _]]->
			{yes, [C1, C2, C3, C4, C5]}
	end.

%% @spec -> RankGroups
%% RankGroups =[[#ecard]], Количество групп в RankGroups <=2
%% Сортировка:
%% 1) по убыванию количества карты в группе
%% 2) по убванию ранга карты.
%% Таким образом если есть каре, то эта группа находиться слева.
%% Если есть две пары, то старшая пара находитсья слева.
get_rank_groups(Cards7)->
	AllGroups=[[C || C <- Cards7, C#ecard.rank=:=Rank]
	           || Rank <- lists:seq(14, 2, -1)],
	RankGroups=lists:reverse(lists:sort(
	                           fun (G1, G2)->
			                           length(G1)<length(G2)
	                           end,
	                           [G || G <- AllGroups, length(G) > 1]
	                          )),
	case RankGroups of
		[RG1, RG2|_ ]->
			[RG1, RG2];
		_->
			RankGroups
	end.
%%% ===========================================
%%%   Определение силы руки
%%% -------------------------------------------
%% @doc охватывает ситуации:
%% * флеш рояль
%% * флеш стрит
%% * и флеш и стрит (но не не флеш стрит)
%% * стрит
flush_royal_or_less(Cards7)->
	case maybe_straight(Cards7) of
		no->
			quads_or_less(Cards7);
		{yes, no_flush, Cards5}->
			case maybe_flush(Cards7) of % Пойми: здесь maybe_flush -- это проверка комбинации "Флеш", а не "флеш-стрит"
				no->
					return_straight(Cards5);
				{yes, Cards5Flush}->
					return_flush(Cards5Flush)
			end;
		{yes, flush, Cards5}->
			[#ecard{rank=RankFirstCard} | _]=Cards5,
			case RankFirstCard of
				14->
					return_flash_royal(Cards5);
				_->
					return_flush_straight(Cards5)
			end
	end.

quads_or_less(Cards7)->
	RankGroups=get_rank_groups(Cards7),
	case RankGroups of
		[]->
			flush_or_less(Cards7, RankGroups);
		[FirstRankGroup |_] ->
			case length(FirstRankGroup)=:=4 of
				false->
					full_house_or_less(Cards7, RankGroups);
				true->
					return_quads(FirstRankGroup, Cards7)
			end
	end.

full_house_or_less(Cards7, RankGroups)->
	case RankGroups of
		[FirstRankGroup, SecondRankGroup]->
			case length(FirstRankGroup) of
				3->
					[Card1, Card2 | _MayBeMoreCards]=SecondRankGroup,% ставим слева | _] на случай если чувак собрал две тройки.
					return_full_house(FirstRankGroup, [Card1, Card2]);
				_->
					flush_or_less(Cards7, RankGroups)
			end;
		_->
			flush_or_less(Cards7, RankGroups)
	end.

%% @doc охватывает ситуации:
%% * флеш
%% * тройка
%% * две пары
%% * пара
%% * старшая карта
flush_or_less(Cards7, RankGroups)->
	case maybe_flush(Cards7) of
		no->
			case RankGroups of
				[]->
					return_high_card(Cards7);
				[First | _] when length(First)=:=3 ->
					return_three_of_a_kind(First, Cards7);
				[First] ->
					return_pair(First, Cards7);
				[First, Second] ->
					return_two_pairs(First, Second, Cards7)
			end;
		{yes, Cards5Flush}->
			return_flush(Cards5Flush)
	end.

%%% ===========================================
%%%   Определение силы руки - return
%%% -------------------------------------------
%% return_flash_royal(Cards5)->
return_flash_royal(Cards5)->
	#force{type = 9,
	      ecards = Cards5
	     }.

%% return_flush_straight(Cards5)->
return_flush_straight(Cards5)->
	#force{type = 8,
	      ecards = Cards5
	     }.

%% return_quads(Cards4, Cards7)->
return_quads(Cards4, Cards7)->
	#force{type = 7,
	      ecards = get_5_cards(Cards4, Cards7)
	     }.
%% return_full_house(Cards3, Cards2)->
return_full_house([Card1, Card2, Card3], [Card4, Card5])->
	#force{type = 6,
	      ecards = [Card1, Card2, Card3, Card4, Card5]
	     }.

%% return_flush(Cards5)
return_flush(Cards5)->
	#force{type =5,
	      ecards=flush_cards(Cards5)
	     }.
flush_cards(Cards5)->
	flush_cards(Cards5, []).
flush_cards([C|Cards], Acc)
  when C#ecard.type =:= ?CARD1;
       C#ecard.type =:= ?CARD2->
	[C|lists:reverse(Acc)]++Cards;
flush_cards([C|Cards], Acc)->
	flush_cards(Cards, [C|Acc]);
flush_cards([], Acc) ->
	lists:reverse(Acc).

%% return_straight(Cards5)
return_straight(Cards5)->
	#force{type =4,
	      ecards = Cards5
	     }.

%% return_three_of_a_kind(Cards3, Cards7)
return_three_of_a_kind(Cards3, Cards7)->
	#force{type = 3,
	      ecards = get_5_cards(Cards3, Cards7)
	     }.
%% return_two_pairs(Cards2High, Cards2, Cards7)
return_two_pairs([Card1, Card2], [Card3, Card4], Cards7)->
	#force{type =2,
	      ecards = get_5_cards([Card1, Card2, Card3, Card4], Cards7)
	     }.
%% return_pair(Cards2, Cards7)
return_pair(Cards2, Cards7)->
	#force{type =1,
	      ecards = get_5_cards(Cards2, Cards7)
	     }.
%% return_high_card(Cards7)
return_high_card([Card1, Card2, Card3, Card4, Card5, _Card6, _Card7])->
	#force{type =0,
	      ecards=[Card1, Card2, Card3, Card4, Card5]
	     }.

get_5_cards(CardsComb, Cards7)->
	[C1, C2, C3, C4, C5| _]=CardsComb++(Cards7--CardsComb), % т.е. переместили CardsComb в начало
	[C1, C2, C3, C4, C5].

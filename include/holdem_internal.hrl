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

-type cur_timer()::{warning|prewarning, integer()}.%% mseconds
-record(ecard,{ % extra card
          type, % 1 | 2 | 4 | 8 | ... | 64
          rank, % 2 | 3 | ... | 14 (туз)
          suit % 0(крести)| 1(буби)| 2(черви) | 3(пики)
         }).

-record(force, {
          type, % тоже, что и #comb.type
          ecards::[#ecard{}]
         }).
-record(p, {
          %% #p пристутствует только в одном списке, за исключением
          %% списка #preblinds, в котором могут быть копии некоторых
          %% игроков.

          %% common ==========
          seat,
          stack = 0,

          %% game ============
          bet = 0,
          %% если G==#fgame, #p.bet то означает размер выигрыша
          %% (фишки, передвинутые к игроку, но еще не зачисленные на в
          %% стек
          batches = [0], % for #game
          card1 = false, % = false | card()
          card2 = false, % = false | card()
          premove = false,
          %% = false | CurBet - значение CurBet, котором была
          %% отправлена информация для предварительного хода
          last_action=false::false
                           | sb | bb| sbb
                           | fold | call | check | raise
                           | weaker | weaker_comb | comb,
          %% делал ли ход в текущем круге торговли

          %% showdown ============
          comb = false::false | #comb{}, % for snapshot
          force::#force{}, % #force | false
          is_open = false, % открыл ли игрок карты

          %% nogame ==========
          skipsb = false::boolean(),
          skipbb = false::first_game_penalty|boolean(),

          %% other ====
          info::any() % for snapshot
         }
       ).
-record(bank, {
          chips=0,
          is_disabled_rake = false,
          %% true, значит рейк нельзя брать рейк, иначе, если
          %% length(#bank.players) /=1, то можно
          players=[]
          %% = [#p] Игроки уровнявшие только этот банк. Если список
          %% пуст, а chips > 0, то на банк претендуют все неаллин
          %% игроки
         }).

-record(bet, {
          seat,
          bet = 0,
          batches = []
         }).

%%% ===========================================
%%%   #sd{}
%%% -------------------------------------------
-record(info, {
          sb, bb,
          ante::false | float(),
          %% размеры ставок в тёмную

          max_players::integer(), % for snapshot

          autostart::boolean(), %% переменная проверяется в момент, когда нужно начать новую игру
          callback, %% {Module, Function, Args}. elrang:apply(Module, Function, [ holdem_out() | holdem_next() | Args])

          waiting = false::false | {prewarning | warning,  reference()},
          %% ждем ответ от игрока

          rake=false::false | float(),
          autoblinds::boolean(),
          penalty_new_player::false | true | after_next_game, % OLD: mark_skipbb
          %% = false | true | first_game (еще не было игры)
          %% Помечать новых игроков как skipbb.
          %% (только если за столом уже есть 2 игрока и уже была игра)

          button::undefined|seat(), % положение баттона, которое видят клиенты
          sb_seat::undefined | seat(),
          bb_seat::undefined | seat(),
          %% sb_seat, bb_seat обновляются в функциях pay_blind,
          %% not_pay_blind



          autoplayer_strategy::blind_check_fold | noblind_check_fold,
          autoplayers = []::[seat()],
          %% списки игроков, за который делается автоход

          unseat_players = []::[seat()],
          %% списки игроков, которых нужно выкинуть по завершению игры

          players = []::[#p{}],
          %% pregame: список игроков, отказавшихся платить блайнд,
          %% игроки с нулевым стеком и другие игроки пропускающие игру
          %%
          %% game: список игроков, не имеющих карт.
          %%
          %% nogame: список игроков
          %%
          %% В этот список добавляются новые игроки. В этом списке
          %% ищется игрок, запросивший boast

          banks = [#bank{}]::[#bank{}],
          %% = [#b] Сортировка: нулевой банк справа. В начале списка -
          %% текущий банк с #bank.players==[]

          allin = [],
          %% = [#p] - алл-ин игроки текущего круга торговли.

          dead = [],
          %% = [#bet] - Мертвые фишки - игрок сделал ставку, но вышел
          %% из игры (fold или unseat)

          timers::#holdem_timers{}
         }
       ).

%% Сбор блайднов
-record(pregame,{
          cur::sb | bb | sbb, % OLD: #info.cur
          %% какой тип блайндов должен заплатить текущий игрок

          cur_req_blind::#req_blind{},
          cur_timer::{integer(), cur_timer()}, %% unixtime

          cur_seat::undefined|seat(), % кому отправлен запрос на уплату

          sb,
          %% = #p - Игрок, которому отправлен запрос на уплату МБ
          %%
          %% | [#p] Случай, когда МБ будет первый кто согласится
          %%        заплатить ( в предыд игре не было ББ). В этот
          %%        список попадут все игроки кроме баттона.
          %%
          %%        Если из них никто не заплатит, то игра не
          %%        начнется. После уплаты оставшиеся игроки
          %%        перемещаются в #pregame.bb.
          %%
          %% | false (нет МБ)
          %%
          %% | true (МБ уплачен)

          bb,
          %% = [#p] Список потенциальных ББ. ББ будет игрок, который
          %%        первым согласится заплатить. После уплаты
          %%        оставшиеся игроки перемещаются в penalty.
          %%
          %%        Если список стал пустым (изначально был пустым или
          %%        никто не заплатил), то если не было МБ, игра не
          %%        начнётся, иначе игра будет без ББ
          %%
          %% | true - ББ уплачен, нужно проверять штрафных)
          %% | false - Игра без ББ. Осталось проверить шрафы у баттона
          %% | undefined - В случае когда #pregame.sb=[#p]

          penalty,
          %% = [#p]
          %%
          %% До того как определен ББ в списке хранится игрок
          %% баттоне, либо это пустой список (head's up).
          %%
          %% После уплаты ББ список пополняется слева игроками,
          %% оставшимиеся в списке bb и, таким образом, представляет
          %% собой список игроков, которых надо проверить на наличие
          %% штрафов. Когда этот список заканчивается сразу начинается
          %% раздача карт.

          ingame = []
          %% = [#p] Заплатили блайнд либо не должны были.
         }).



-record(game, {
          board_cards,% = [Flop1, Flop2, Flop3, Turn, River] %OLD: cards

          opened_board_cards = preflop,
          %% = preflop | flop | turn | river
          cur_you_move::{seat(), #you_move{}},
          cur_timer::{integer(), cur_timer()}, %% unixtime

          rake_chips = 0, % - сколько собрано рейка

          cur_bet,
          delta_raise, % максимальное изменение ставки

          min_raise, % = cur_bet + max_delta_raise
          max_raise, % = #p.bet + #p.stack
          %% Если min_raise > max_raise (не хватает фишек на
          %% минимальное повышений), то делается правка
          %% max_raise=min_raise
          %%
          %% Если игрок не может повышать (см holdem.hrl:
          %% #you_move.chips), то min_raise=1, max_raise=0

          wait,
          %% = [#p] - еще не ходили.Сортировка: текущий игрок в начале
          %% списка.

          done = []
          %% = [#p] Остались в игре. После повышения текущей
          %% ставки весь список перемещается в wait.
          %% Сортировка: новые игроки добавляются слева.

         }).
%% Showdown
-record(fgame, { % final of game
          board_cards::[card()], % for snapshot.
          cur_highlight=false::false|{seat(), highlight()},

          %% текущий банк
          chips::chips(),

          used_cards = 0::0..5 , % сколько карт использовано при определении текущей лучшей руки

          is_single_player=false::boolean(), % true, если все остальные игроки упали

          %% игроки текущего банка
          players = []::[#p{}], %% на очереди
          winners =  []::[#p{}],
          losers = []::[#p{}] % #p.bet может быть ненулевым, если игрок выиграл предыдущий банк
         }).


-record(preblinds,{
          %% копии игроков
          pregame::#pregame{},
          no_active_players::[#p{}]
         }).
-record(nogame, {
          timer = false::false | button | #preblinds{},
          %% #preblinds -- баттон уже передвинут, но ждем начало
          %% игры. Если за это время изменится состав активных
          %% игроков, то #pregame{} и положение баттона будет
          %% перерасчитано.
          %%
          %% button -- запущен таймер, после которого будет
          %% вычислен и передвинут баттон
          %%
          %% false -- нет таймеров, запускающих игру

          force_button = false::false | seat(),
          %% seat() -- задается при вызове holdem:button/2 или при
          %% входе первого игрока -- после старта новой игры блайнды
          %% определяются исходя из указанного положения баттона,
          %% затем перепроверяется правильность положения баттона (на
          %% случай, если указанный игрок не принимает участие в игре

          %% сохраняются из #info в момент создания #nogame и больше
          %% не изменяются
          sb_seat_old::undefined|seat(),
          bb_seat_old::undefined|seat(),
          button_old::undefined|seat()

         }).

-type sd()::{#info{},
               #pregame{} | #game{} | #fgame{} | #nogame{} }.
%%% ===========================================
%%%   Internal defenitions
%%% -------------------------------------------
-type ontimer() ::

        button |
        start_blinds | % OLD: blinds
        next_blind |
        dealing_cards |
        preflop |

        new_round_next_move |

        collect_bets |
        open_board |
        clear_moves |
        start_showdown |

        on_switch_to_nogame |

        %single_player_move_bank |
        %single_player_clear_winner_chips |
        %single_player_clear_cards |

        showdown_next |
        showdown_clear_losers |
        showdown_clear_winner_chips |
        showdown_clear_board_cards |
        showdown_clear_winners |
        showdown_onclear.


-type timer()::{integer(), ontimer()}. %% пауза-действие
-record(done, {
          cur = unchanged::update_cur_player(),
          cur_timer::cur_timer(),
          timers = []::[timer()], %% таймеры для последовательного запуска
          broadcast=[]::broadcast(),
          sd::sd()
              }).
-define(RAKE(Chips, RakeFactor), (trunc(Chips * RakeFactor))).
-define(EXTEND_BROADCAST(Record, Done), Done#done{broadcast=[Record|Done#done.broadcast]}).
-define(HNOUT(Out), #holdem_next{holdem_out=Out}).
-define(TIMER (I, Timer), (I#info.timers)#holdem_timers.Timer).


%% DEBUG
%-define(DEBUG(Format, Args), io:format(lists:concat(['~n==[DEBUG]================~n', ?MODULE, "(", ?LINE, ")~n~n", Format, "~n~n--[debug]----------------~n"]), Args)).
-define(DEBUG, lager:debug(Format, Args)).


-define(RECORD(R,Name), lists:zip(record_info(fields,Name),tl(tuple_to_list(R)))).
-define(INFO(I), ?DEBUG("I#info=~p", [lists:zip(record_info(fields,info),tl(tuple_to_list(I)))])).
-define(PLAYERS(Label, Players), ?DEBUG(Label++"=~p", [[lists:zip(record_info(fields,p),tl(tuple_to_list(P)))||P<-Players]])).
%lists:zip(record_info(fields,Name),tl(tuple_to_list(R)))

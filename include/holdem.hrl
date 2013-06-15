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

%%% ===========================================
%%%   holdem_options
%%% -------------------------------------------

-record (blinds, {
           sb::integer(),
           bb::integer(),
           ante = false::false|integer()
                       }).
-record(holdem_timers,{
          %% Начало новой игры. (Таймеры запускаются последовательно)
          after_ante = 0::integer(),
          before_button_onplayers = 0::integer(), % Если autostart==true и собралось достаточно игроков
          before_button = 0::integer(), % Если autostart==true и только что стол очищен от предыдущей игры
          before_blinds = 0::integer(),

          %% если игрок не успевает сходить, то за него делается ход
          %% autocheck:
          blind_prewarning = 0::integer(),
          blind_warning = 5000::integer(),
          move_prewarning = 0::integer(),
          move_warning = 5000::integer(),

          after_autoblinding = 0::integer(), % время на 1 человека
          after_dealing = 0::integer(), % пауза перед первым ходом

          %% 1 банк. Перед новым кругом.
          after_round = [{0, collect_bets}, {0, open_board}, {0, clear_moves}]
         ::[{integer(), atom()}],
          %% [{Delay, Action}]. -- сначала делается паука, потом
          %% выполняетя действие и т.д.. Пауза может равняться
          %% нулю. Сортировка: первое дейтвие -- то, которое слева. В
          %% списке должные быть представленны все действия.
          %%
          %% Action:
          %% * collect_bets (только если были ставки, иначе паузы не будет)
          %% * open_board
          %% * clear_moves (убрать таблички "чек", "колл" и т.д (кроме аллинов))
          before_new_round = 0::integer(),
          %% после выполнения after_round делается пауза перед тем как
          %% начнутся ходы

          %% 1 банк. Перед вскрытием.
          %% Таймеры запускаются параллельно.
          after_river = [{0, collect_bets}, {0, clear_moves}]::[{integer(), atom()}],
          %% Action:
          %% * collect_bets (только если были ставки)
          %% * clear_moves (убрать таблички "чек", "колл" и т.д, кроме аллины)
          before_showdown = 0::integer(), % после выполнения after_river

          %% Мультибанк.(Таймеры запускаются последовательно)
          %% [Без паузы] fix_batches, fix_batches_crazy
          multipot_before_collecting_bets = 0::integer(),
          multipot_after_collecting_bets = 0::integer(), % время на 1 банк
          %% [Без паузы] (             , если не все аллин) убираются таблички хода (кроме аллинов)
          %% [Без паузы] (если    ривер,                  ) начинается вскрытие
          %% [Без паузы] (если не ривер, если не все аллин) открывается следующая карта стола
          %% [Без паузы] (если не ривер, если не все аллин) начинается круг торговли
          %% [Без паузы] (если не ривер, если    все аллин) все все открывают карты
          %%             (если не ривер, если    все аллин) открывается следующая карту стола
          before_opening_board_allin_first = 0::integer(),
          %%             (если не ривер, если    все аллин) открываются посследующие карты стола
          before_opening_board_allin = 0::integer(),
          %%             (если    ривер, если    все аллин, все уже открылись) начинается определение победителя
          before_showdown_allin = 0::integer(),


          %% Если все упали и остался только один игрок:
          single_player_before_collect_bets = 0::integer(),
          single_player_clear_table = [
                                       {0, clear_moves},
                                       {0, showdown_next}, % before move bank
                                       {0, showdown_clear_winner_chips},
                                       {0, showdown_clear_board_cards},
                                       {0, showdown_clear_winner_cards}
                                      ]
          ::[{integer(), atom()}],
          %% Action:
          %% * showdown_clear_losers (карты и таблички) -- те, кто не выиграл нулевой
          %% * showdown_clear_winner_chips (убрать фишки победителя в стек)
          %% * showdown_clear_board_cards
          %% * showdown_clear_winner_cards (карты и таблички)

          %% Showdown
          after_player_showdown = 0::integer(), % если это не последний игрок
          after_player_weaker = 0::integer(), % если это не последний игрок
          before_move_bank_to_winner = 0,  %% передвинуть банк победителю

          %% Showdown. Следующий банк. (Таймеры запускаются последовательно)
          before_bank_losers = 0::integer(), % #clear_showdown_players clear_winner_chips
          before_showdown_next_bank = 0::integer(),
          before_showdown_next_bank_after_single_player = 0::integer(),  %% пауза после выдача банка единственному игроку, который уровнял банк ("сдача")

          %% Уборка стола.
          clear_table=[{0, showdown_clear_losers},
                       {0, showdown_clear_winner_chips},
                       {0, showdown_clear_board_cards},
                       {0, showdown_clear_winner_cards}]
          ::[{integer(), atom()}]
          %% Action:
          %% * showdown_clear_losers (карты и таблички) -- те, кто не выиграл нулевой
          %% * showdown_clear_winner_chips (убрать фишки победителя в стек)
          %% * showdown_clear_board_cards
          %% * showdown_clear_winner_cards (карты и таблички)
}).

-record(holdem_options, {
          autoblinds::boolean(),
          autoplayer_strategy::blind_check_fold | noblind_check_fold,
          autostart::boolean(), %% автоматически начинать игру, если достаточно игроков.
          blinds::#blinds{},
          max_players::integer(),
          penalty=true::boolean(),
          rake = false::false|float(),

          callback,
          %% {Module, Function, Args}.
          %% elrang:apply(Module, Function, [Req | Args])
          %% Req:
          %% * #holdem{}
          %% * {auto_unseat, [{seat(), stack()}]}
          timers::#holdem_timers{}
                                 }).

%%% ===========================================
%%%   types
%%% -------------------------------------------

-type blind_type():: sb | bb | sbb.%% sbb = small+big blind

-type seat()::0..9.
-type chips()::integer().
-type stack()::chips().
-type batches()::[integer()].
%% Фишки, разложенные по кучкам. Сортировка: кучка номер 0
%% слева.  Кучка номер N означает чему равна N-ая стопка,
%% или сколько нужно добавить фишек в N-ую стопку. Если
%% число отрицательное, значит нужно уменьшить число фишек в
%% данной стопке.

-record(bet_data, {
          seat::seat(),
          batches=[]::batches(),

          allin = false::0..9
          %% номер банка, который уровнял аллинщик или false, если игрок
          %% не аллин
         }).

-type card()::0..51.
%% card=suit*13 + rank-2,
%%
%% rank = 2 | 3 | ... | 9 | 10 | 11(валет) | 12(дама) | 13(король) |
%% 14(туз)
%%
%% suit = 0(крести)| 1(буби)| 2(черви) | 3(пики)

%%% ===========================================
%%%   broadcast
%%% -------------------------------------------
-record(clear_moves, {}).%% убрать таблички с ходом (кроме аллина)
-record(board_cards, {level::flop | turn | river,
                      cards::[card()] % 3 or 1
                             }).
-record(player_leave, {seat}).
-record(new_player, {seat, stack, info::any()}).
-record(new_stack, {seat,
                    stack,
                    add}). % сколько добавилось}).

%% @type button() = #button{seat=undefined|seat()}.
%% Новая позиция буттона
-record(button, {seat}).

%% @type deal_cards() = #deal_cards{
%%    cards=[{Seat, Card1, Card2}]}.
%% Раздача карт.
%% Cards - перечисляются начиная со следующего от буттона
%%-record(deal_cards, {cards}).
%% OLD:dealing_cards

-record(deal_cards_viewer, {seats}).
-record(deal_cards_player, {seat, card1, card2, seats}).

%% сортировка rakes: слева рейк для банка номер 0
-record(collect_bets, {rakes::[chips()]}).

%% @type req_blind() = #req_blind{
%%    seat=seat(),
%%    type=blind_type(),
%%    is_all_in=bool()}.
%% Спросить будет ли игрок платить блайнд.
-record(req_blind, {seat, type, chips, is_all_in = false}).

-record(pay_blind, {type::blind_type(),
                    to_bank=0::chips(),
                    %% сколько фишек (дополнительно к data), сразу
                    %% уходит в банк

                    data::#bet_data{}
                   }).

%% @type not_pay_blind() = #not_pay_blind{
%%    seat=seat()}.
%% Игрок не платит блайнд.
-record(not_pay_blind, {seat}).

-type ante_value()::{chips(), [seat()]} %% если нет аллин
                  | [#bet_data{}]. %% если есть аллин
-record(ante, {value::ante_value()}).

%% @type blind_death() = #blind_death{return_blind = Return}
%% Return = false | {seat(), chips()}.
%%
%% Игра не может начаться, т.к. не хватает игроков: все отказались
%% платить блайнд. Если был 1 "блайндер", то его фишки возвращаются.
-record(blind_death, {return_blind=false}).

%% @type now_move() = #now_move{seat=seat(), seconds}.
%% Номер игрока, который сейчас ходит.
-record(now_move, {seat::seat(),
                   mseconds=false::false | integer()}).

%% @type precall() = #precall{seat = seat(),
%%                           call_add = chips(),
%%                           is_all_in = bool()}.
%%
%% Игрок может подумать о предварительном решении хода - колл или пас.
%% Когда до него дойдет ход клиент автоматически отправит решение о ходе.
%%
%% seat - кому предназначено сообщение
%%
%% call_add -  сколько нужно добавить, чтобы уровнять
%%
%% is_all_in - если уровнять можно толька идя ва-банк
%%
%% Отправляется только если это начало круга
%% торговли, либо изменился размер текущей ставки.
-record(precall, {
          %seat,
          call_add,
          is_all_in}).

%% @type precheck() = #precheck{seat=seat()}.
%%
%% Игрок может подумать о предварительном решении хода - чек или пас.
%% Когда до него дойдет ход клиент автоматически отправит решение о ходе.
%%
%% seat - кому предназначено сообщение
%%
%% Отправляется только если это начало круга торговли.
-record(precheck, {
          %seat
         }).

%% @you_move() = #you_move{chips=Chips}
%%
%% Chips = [CallAdd] |
%%         [CallAdd, 0] |
%%         [CallAdd, MinRaiseTo] |
%%         [CallAdd, MinRaiseTo, MaxRaiseTo].
%%
%% 1) [CallAdd] - Игрок может сходить только "call", но при этом не
%% ставя всех своих фишек (не алл-ин). Происходит когда единственный
%% сопперник пошел алл-ин и данному игроку нет смысла повышать, он
%% может только уровнять. Либо другой игрок пошел алл-ин, но не
%% хватило до минимального повышения и, таким образом, у данного игрок
%% нет права делать рейз
%%
%% 2) [CallAdd, 0] - Ход "уравнять" будет ставкой алл-ин
%%
%% 3) [CallAdd, MinRaiseTo] - повысить игрок сможет только до MinRaiseTo,
%% поставив алл-ин.
%%
%% 4) [CallAdd, MinRaiseTo, MaxRaiseTo]
%%
%% CallAdd - сколько нужно добавить, чтобы уровнять. Если равно 0,
%% значит это ход "чек"
%%
%% MinRaiseTo - означает минимальную ставку, до которой возможно
%% повышение.
%%
%% MaxRaiseTo - Максимальная ставка (стек + текущая ставка
%% игрока).
-record(you_move, {chips}).

%% "Проснись" -- игроку засчитан автоход, т.к. он не сходил вовремя
-record(wakeup, {}).

-record(show_player_move, {
          type::autofold | fold | check | call | raise,
          data::#bet_data{}
}).

%% Используется если нужно переразложить стопки фишек в ставке игрока
-record(fix_batches, {
          data::#bet_data{},
          delta=0::chips()
          %% Если delta отлично от нуля, значит в ставке стало меньше
          %% фишек, которые взимаются как рейк. Используется только
          %% для игроков, которые сделали ставку и вышли из игры
         }).

%% @type finish_betting() = #finish_betting{
%%    next_round = NextRound,
%%    bank = Bank,
%%    rakes = Rakes
%% }
%% NextRound = {flop, Flop1, Flop2, Flop3} |
%%             {turn, card()} |
%%             {river, card()} |
%%             after_river |
%%             {premature_showdown, Cards},
%% Rakes = [chips()]
%% Bank = banknum().
%%
%% Bank: i-ая стопка каждого игрока уходит в банк номер Bank+i.
%% Сортировка Rakes рейк для банка номер Bank расположен слева.
%%
%% premature_showdown:
%%
%% Cards - 0(вскрытие общий карт не будет - остался только 1 игрок)
%% 2(после флопа) или 5 карт(после префлопа). В остальных случай
%% отправляет {river, card()} или after_river
%%
%% На клиенте должны отменятся действия now_move, you_move (т.к. нет
%% спец команды в случае unseat и смены хода)
%% -record(finish_betting, {next_round, bank, rakes=[]}).

%% Используется если все ушли в алл-ин или все упали кроме одного и он
%% решил показать карты (boast).
%% OLD^ player_cards
-record(open_player_cards, {seat, card1, card2}).

%% @type req_boast() = #req_boast{seat=seat()}.
%% Спросить будет ли игрок открывать свои карты ("хвастаться").
-record(req_boast, {}).

%% @type showdown() = #showdown{
%%    seat = seat(),
%%    cards = {open, Card1, Card2, Combination, Highlight} |
%%            {repeat, Combination, Highlight} |
%%            weaker % OLD: weak
%% }
%% Combination = comb()
%% Highlight = integer().
%%
%% Highlight - битовая маска карт для "подсветки"
%%
%% repeat используется для случаев, когда карты игрока уже открыты
%% (было вскрытие карт до торна).
%%
%% +1 флоп1
%% +2 флоп2
%% +4 флоп3
%% +8 торн
%% +16 ривер
%% +32 card1
%% +64 card2
%%-record(showdown, {seat, cards}).

%% @type comb() = #comb{
%%   type = Type,
%%   cards = [Rank]
%% }.
%% Type - код кобминации (0 - старшая карта, ..., 9 - флеш рояль)
%%
%% Rank - ранк карты, число от 2 до 14.
%%
%% cards - нет дубликатов. Например [c1] - для каре, [c1,c2] - для
%% фулл хауса
%%
%% В комбинации могут быть опущены несколько последних кикеров или все
%% кикеры, если они не участвовали в определении силы.
%%
%% <table><tbody>
%% <tr><td> Код </td><td> Название        </td><td> Rank1  </td><td> Rank2 </td><td> Rank3 </td><td> Rank4 </td><td> Rank5 </td></tr>
%% <tr><td>   9 </td><td> Флеш-рояль      </td><td> c1     </td><td> c2    </td><td> c3    </td><td> c4    </td><td> c5    </td></tr>
%% <tr><td>   8 </td><td> Стрит-флеш      </td><td> c1     </td><td> c2    </td><td> c3    </td><td> c4    </td><td> c5    </td></tr>
%% <tr><td>   7 </td><td> Каре            </td><td> c1     </td><td> c1    </td><td> c1    </td><td> c1    </td><td> k1    </td></tr>
%% <tr><td>   6 </td><td> Фулхаус         </td><td> xx     </td><td> xx    </td><td> xx    </td><td> yy    </td><td> yy    </td></tr>
%% <tr><td>   5 </td><td> Флеш            </td><td> c0*    </td><td> c1    </td><td> c2    </td><td> c3    </td><td> c4    </td></tr>
%% <tr><td>   4 </td><td> Стрит           </td><td> c1     </td><td> c2    </td><td> c3    </td><td> c4    </td><td> c5    </td></tr>
%% <tr><td>   3 </td><td> три одного типа </td><td> c1     </td><td> c1    </td><td> c1    </td><td> k1    </td><td> k2    </td></tr>
%% <tr><td>   2 </td><td> две пары        </td><td> c1     </td><td> c1    </td><td> c2    </td><td> c2    </td><td> k1    </td></tr>
%% <tr><td>   1 </td><td> пара            </td><td> c1     </td><td> c1    </td><td> k1    </td><td> k2    </td><td> k3    </td></tr>
%% <tr><td>   0 </td><td> [старшая карта] </td><td> c1     </td><td> k1    </td><td> k2    </td><td> k3    </td><td> k4    </td></tr>
%% </tbody></table>
%%
%% Одиннаковыми символами обозначены карты одного ранга
%%
%% с1, с2 и т.д. означает карта комбинации с наивысшим рангом, со
%% вторым рангом и т.д.
%%
%% k1, k2 и т.д. - кикер с наивысшим рангом, со вторым рангом и т.д.
%%
%% * c0 - старшая карманная карта, участвовавшая в комбинации флеш,
%% либо самая младшая карта за столом.
%%

-type highlight()::integer().
%% битовая маска карт для "подсветки"
%% +1 флоп1
%% +2 флоп2
%% +4 флоп3
%% +8 торн
%% +16 ривер
%% +32 card1
%% +64 card2
-type comb_code()::0..9. %% старшая карта, пара, ... , флеш рояль
-record(comb, {type::comb_code(),
               ranks::[2..14],
               highlight::highlight()}).
%% назвать комбинацию и подсвятить
-record(showdown_stronger,{
          seat::seat(),
          comb::#comb{},
          card1::card(),
          card2::card()
                 }).

%% карты уже открыты, т.к. было досрочное выскрытие карт
%% назвать комбинацию и подсвятить
-record(showdown_stronger_allin,{
          seat::seat(),
          comb::#comb{}
         }).
%% назвать комбинацию и подсвятить (если есть comb)
-record(showdown_weaker,{
          seat::seat(),
          comb=false::false|#comb{} %% для игрока-владельца или если его карты уже открыты
         }).
%% назвать комбинацию, но не подсвечивать
-record(showdown_weaker_boast,{
          seat::seat(),
          card1::card(),
          card2::card(),
          comb::#comb{}
                }).
%% назвать комбинацию, но не подсвечивать
-record(showdown_update_comb,{
          seat::seat(),
          comb::#comb{}
          % OLD: value::[{seat(),#comb{}}]
         }).
%% @type bank_for_winner() = #bank_for_winner{
%%   seats = [seats()],
%%   chips = chips()
%% }.
%% chips - сколько фишек получает каждый победитель.
%% Номер банка -- наивысший номер из известных клиенту
%% Список игроков может быть пустым (unseat)
-record(bank_for_winner, {seats=[], chips=0}).

%% @type bank_losers() = #bank_losers{seats=[seat()]}.
%%
%% Клиент получает список игроков, которые претендовали на текущий
%% банк, но не выиграли и таким образом не участвуют в розыгрыше
%% следующего банка (банк с меньшим номером).
-record(bank_losers, {seats}).

%% @type end_game() = #end_game{rake = chips(),
%%                              stacks = [{seat(), chips()}]
%%                             }.
%%
%% rake - сколько фишек собрано в виде рейка.
%%
%% stacks - сколько фишек у каждого из игроков.
%-record(end_game, {rake, stacks}).

%% зачислить фишки победителю
-record(clear_winner_chips,{
          stacks::[{seat(), stack()}]
                    }).

% убрать карты стола
-record(clear_board_cards, {}).

% убрать карты и таблички хода
-record(clear_player_cards, {seats::[seat()]}).


-type broadcast_to()::seat() | {except, seat()} | viewers | all. % default: all
-type broadcast_msg()::#ante{}
                          | #bank_for_winner{}
                          | #bank_losers{}
                          | #board_cards{}
                          | #blind_death{}
                          | #button{}
                          | #clear_board_cards{}
                          | #clear_moves{}
                          | #clear_player_cards{}
                          | #clear_winner_chips{}
                          | #collect_bets{}
                          | #deal_cards_viewer{}
                          | #deal_cards_player{}
                          | #fix_batches{}
                          | #new_player{}
                          | #new_stack{}
                          | #not_pay_blind{}
                          | #now_move{}
                          | #pay_blind{}
                          | #open_player_cards{}
                          | #player_leave{}
                          | #precall{}
                          | #precheck{}
                          | #req_blind{}
                          | #req_boast{}
                          | #show_player_move{}
                          | #showdown_stronger{}
                          | #showdown_stronger_allin{}
                          | #showdown_weaker_boast{}
                          | #showdown_weaker{}
                          | #showdown_update_comb{}
                          | #you_move{}
                          | #wakeup{}.

-type broadcast()::[{ broadcast_to(), broadcast_msg()} | broadcast_msg()].
%%% ===========================================
%%%   snapshot
%%% -------------------------------------------

-record(holdem_snapshot_player, {
          seat,
          info::any(),
          stack,
          autoplayer::boolean(), %% TODO
          cards::true | false | {card(), card()},
          move::false
              | sb | bb| sbb
              | fold | call | check | raise
              | {allin, integer()} % номер банка, который уровнял аллинщик
              | weaker | #comb{} | {weaker, #comb{}}
         }).
%% @type holdem_snapshot() = #holdem_snapshot{
%%   sb=chips(),
%%   bb=chips(),
%%   ante=chips() | false,
%%   button = undefined | seat(),
%%   board_cards =  BoardCards,
%%   banks = [chips()],
%%   bets = [{seat(), batches()}],
%%   players = [ssp()]
%% }
%% BoardCards = [card()].
%%
%% Снимок состояния стола. Отправляется игроку сразу после подключения
%% к столу.
%%
%% banks - сортировка: нулевой банк слева.
%%
%% BoardCards = 0, 3, 4 или 5 карт стола.
%%
%% bets - ставки перед игроками
-record(holdem_snapshot, {
          sb::chips(), bb::chips(), ante::false|chips(),

          banks::[integer()], %% нулевой банк справа
          bets::[{seat(), batches()}],
          board::[card()],
          button::undefined|seat(),
          highlight::false | {seat(), highlight()},
          max_players::integer(),
          owner::false | {seat(), no_cards} | {seat(), card(), card()},
          winner_chips::[{seat(), chips()}], % фишки лежащие перед победителями

          players::[#holdem_snapshot_player{}]}).

%%% ===========================================
%%% Вход
%%% -------------------------------------------
-type holdem_in() ::
        {pay_blind, boolean()}
        | {move,
           autocheck % check or fold
           | fold
           | call % call or check
           | integer()}.% raise_to
%%% ===========================================
%%% Выход
%%% -------------------------------------------
%% от кого ждем holdem_in
-type update_cur_player()::unchanged | % использовать прежнее значение
                             nobody | % никто
                             {seat(), pay_blind | move}.
-record(holdem, {
          cur = unchanged::update_cur_player(),

          broadcast = []::broadcast()
         }).

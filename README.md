This erlang application can be used in any type of unlimited holdem games: SnG, MTT, cash tables etc.

# Using

1.  Add this application as dependence in your project
2.  Start application

    >  application:start(holdem).

3.  Start holdem process:

    >   Options =
    >     #holdem_options{blinds=#blinds{sb=5,bb=10},
    >                     autoplayer_strategy = blind_check_fold,
    >                     penalty=true,
    >                     max_players=10,
    >                     timers=#holdem_timers{move_warning=5000},
    >                     callback = {callback_module, callback_fun, [Arg1, Arg2]}
    >                    },
    >   {ok, Pid} = holdem:start(Options),

4.  Make calls 

    >   #holdem{broadcast=Broadcast, cur=CurPlayer} = holdem:next(Pid, {move, call}).

5.  Get callbacks (for example, if current player timeouted and autofolded)

    >   callback_fun(Holdem = #holdem{}, Arg1, Arg2)->
    
    >     ...


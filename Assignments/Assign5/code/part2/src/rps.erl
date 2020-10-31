-module(rps).
-import(broker, [start_link/0]).
-export([start/0, queue_up/3, move/2, statistics/1, drain/3]).

start() -> 
    broker:start_link().

queue_up(BrokerRef, Name, Rounds) ->
    gen_server:call(BrokerRef, {Name, Rounds}, infinity).

move(Coordinator, Choice) ->
    gen_statem:call(Coordinator, {move, Choice}).

statistics(BrokerRef) ->
    gen_Server:call(BrokerRef, statistics).

drain(_, _, _) ->  nope.

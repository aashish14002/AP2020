-module(broker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start(?MODULE, [], []).



init(_Args) ->
    {ok, #{waiting => #{}, ongoing => [], longest => 0}}.

handle_call({Name, Rounds}, From, State) ->
    #{waiting := WaitingPlayer, ongoing := OngoingTeams} = State,
    case maps:get(Rounds, WaitingPlayer, none) of
        {P1Name, P1_ID} ->  NewTeamCoord = {P1Name, P1_ID, Name},% start new cordinator
                        WaitingPlayer = maps:remove(Rounds, WaitingPlayer),
                        OngoingTeams = OngoingTeams ++ [NewTeamCoord],
                        gen_server:reply(P1_ID, {ok, Name, NewTeamCoord}),
                        {reply, {ok, P1Name, NewTeamCoord}, 
                            State#{waiting := WaitingPlayer, ongoing := OngoingTeams}};
        none -> WaitingPlayer = maps:put(Rounds, {Name, From}, WaitingPlayer),
                {noreply, State#{waiting := WaitingPlayer}}

    end.

handle_cast({From, game_over, RoundNo}, State) ->
    #{longest := Longest, ongoing := OngoingTeams} = State,
    case Longest < RoundNo of
        true -> State = State#{longest := RoundNo, ongoing := lists:delete(From, OngoingTeams)};
        false -> State = State#{ongoing := lists:delete(From, OngoingTeams)}
    end,
    {noreply, State}.



handle_info(Msg, State) ->
    io:format("Error: ~p~n",[Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    io:format("code_change: ~p~n", [Return]),
    Return.
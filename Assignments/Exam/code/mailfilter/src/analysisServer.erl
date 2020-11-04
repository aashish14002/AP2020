-module(analysisServer).
-behaviour(gen_server).

-import(filterServer, [start/1]).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([runFilter/1, startFilters/2, stopFilter/1, stopAllFilters/1, removeAnalysis/2]).

start_link(AnalysisData) ->
    gen_server:start(?MODULE, AnalysisData, []).


runFilter(F) ->
    gen_server:cast(F, run_filter).

stopFilter(F) -> 
    gen_server:cast(F, stop).

removeAnalysis(MS, A) ->
    gen_server:cast(MS, {remove_analysis, A}).

stopAllFilters(FilterServers) ->
    lists:foreach(fun stopFilter/1, FilterServers).

startFilters(Mail, FilterD) ->
    TempFilters = [filterServer:start([L, element(1, maps:get(L,FilterD)), Mail, element(2, maps:get(L,FilterD)), none, self()]) || L <- maps:keys(FilterD)],
    FilterServers = [ F || {ok, F} <- TempFilters],
    lists:foreach(fun runFilter/1, FilterServers),
    FilterServers.

init(AnalysisData) ->
    %% filterData : labels => {filt, Data} 
    %%    filt = {simple, filter_fun()} | {chain, [{simple, filter_fun()}]}
    %% filterServers [filterServerIds]
    %% filterResults : labels => {done, Data} | inprogress
    [Mail, FilterD, MS] = AnalysisData,
    FilterServers = startFilters(Mail, FilterD),
    {ok, #{mail => Mail, filterServers => FilterServers, filterData => FilterD, filterResults => #{}, ms => MS}}.

handle_call(stop_filters, _, #{mail := Mail, filterData := FilterD, filterServers := FilterServers, filterResults := FilterResults}=State) ->
    stopAllFilters(FilterServers),
    Result = {Mail, [ {L, maps:get(L, FilterResults, inprogress)}|| L <- maps:keys(FilterD)]}, 
    {reply, Result, State#{filterServers := [], filterData :=#{}}};

handle_call(get_config, _, #{filterData := FilterD, filterResults := FilterResults}=State) ->
    Result = [ {L, maps:get(L, FilterResults, inprogress)}|| L <- maps:keys(FilterD)],
    {reply, {ok, Result}, State}.

handle_cast({add_filter, Label, Filt, Data}, #{mail := Mail, filterServers := FilterServers, filterData := FilterD}=State) ->
    case maps:get(Label, FilterD, none) of
        none -> case Filt of
                    {simple, _} -> [FS] = startFilters(Mail, #{Label => {Filt, Data}}),
                                runFilter(FS),
                                UpdatedFilters = FilterD#{Label => {Filt, Data}},
                                UpdatedState = State#{filterData := UpdatedFilters, filterServers := [FS|FilterServers]},
                                {noreply, UpdatedState};
                    {chain, _} ->[FS] = startFilters(Mail, #{Label => {Filt, Data}}),
                                runFilter(FS),
                                UpdatedFilters = FilterD#{Label => {Filt, Data}},
                                UpdatedState = State#{filterData := UpdatedFilters, filterServers := [FS|FilterServers]},
                                {noreply, UpdatedState};
                    {group, _, _} -> [FS] = startFilters(Mail, #{Label => {Filt, Data}}),
                                
                                runFilter(FS),
                                UpdatedFilters = FilterD#{Label => {Filt, Data}},
                                UpdatedState = State#{filterData := UpdatedFilters, filterServers := [FS|FilterServers]},
                                io:format("code_change ANALYSIS Add_FILTER_GROUP: ~p~n", [State]),
                                {noreply, UpdatedState};
                    {timelimit, _, _} -> [FS] = startFilters(Mail, #{Label => {Filt, Data}}),
                                
                                runFilter(FS),
                                UpdatedFilters = FilterD#{Label => {Filt, Data}},
                                UpdatedState = State#{filterData := UpdatedFilters, filterServers := [FS|FilterServers]},
                                io:format("code_change ANALYSIS Add_FILTER_TIMELIMIT: ~p~n", [State]),
                                {noreply, UpdatedState};
                    _ ->  io:format("code_change ANALYSIS Add_FILTER_NONE: ~p~n", [State]),
                        {noreply, State}
                end;
        _ ->  io:format("code_change ANALYSIS Add_FILTER: ~p~n", [State]),
                {noreply, State}

    end;

handle_cast({F, Label, just, UData}, #{filterResults := FilterResults, filterServers := FilterServers}=State) ->
    % io:format("code_change ANALYSIS JUST DATA: ~p~n", [State#{filterResults => FilterResults#{Label => {done, UData} }}]),
    stopFilter(F),
    {noreply, State#{filterResults => FilterResults#{Label => {done, UData} }, filterServers := lists:delete(F, FilterServers)}};

handle_cast({_, Label, transformed, UMail}, #{filterData := FilterD, filterServers := FilterServers}=State) ->
    #{Label := {_, Data}}=FilterD,
    stopAllFilters(FilterServers),
    UpdatedServers = startFilters(UMail, maps:remove(Label, FilterD)),
    UpdatedState = State#{ mail => UMail, filterServers => UpdatedServers, filterResults => #{Label => {done, Data}}},
    % io:format("code_change ANALYSIS TRANSFORMED: ~p~n", [UpdatedState]),
    {noreply, UpdatedState};

handle_cast({F, Label, unchanged}, #{filterData := FilterD, filterResults := FilterResults, filterServers := FilterServers}=State) ->
    #{Label := {_, Data}}=FilterD,
    stopFilter(F),
    {noreply, State#{filterResults => FilterResults#{Label => {done, Data} }, filterServers := lists:delete(F, FilterServers)}};

handle_cast({_, Label, both, UMail, UData}, #{filterData := FilterD, filterServers := FilterServers}=State) ->
    stopAllFilters(FilterServers),
    UpdatedServers = startFilters(UMail, maps:remove(Label, FilterD)),
    UpdatedState = State#{ mail => UMail, filterServers => UpdatedServers, filterResults => #{Label => {done, UData}}},
    {noreply, UpdatedState};

handle_cast(enough, #{filterServers := FilterServers, ms := MS}=State) ->
    stopAllFilters(FilterServers),
    removeAnalysis(MS, self()),
    io:format("ENOUGH CAST ++++++++++++++++++++++++++++++++++++++++++++++++++++++++: ~p~n", [State]),
    {noreply, State#{filterServers := [],filterResults := #{}}};

handle_cast(stop, #{filterServers := FilterServers}=State) ->
    stopAllFilters(FilterServers),
    io:format("STOP FROM ANALYSIS----------------------------------: ~p~n", [State]),
    {stop, normal, State#{filterServers := [],filterResults := #{},filterData := #{} }};

handle_cast(_, State) ->
    {ok, State}.

handle_info(Info, State) ->
    io:format("Error: ~p~n",[Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    io:format("code_change: ~p~n", [Return]),
    Return.
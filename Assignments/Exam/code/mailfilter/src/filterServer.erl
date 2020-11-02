-module(filterServer).
-behaviour(gen_server).

-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([runFilterFun/4, chainingHelper/2, updatedFilterState/2]).

start(FilterData) ->
    gen_server:start(?MODULE, FilterData, []).

runNewFilter(F) ->
    gen_server:cast(F, run_filter).

stopNewFilter(F) -> 
    gen_server:cast(F, stop).

stopAllFilters(FilterServers) ->
    lists:foreach(fun stopNewFilter/1, FilterServers).

startNewFilters(M, D, L, MrgF, FiltL) ->
    TempFilters = [filterServer:start([L, Filt, M, D, MrgF, self()]) || Filt <- FiltL],
    FilterServers = [ F || {ok, F} <- TempFilters],
    lists:foreach(fun runNewFilter/1, FilterServers),
    FilterServers.

runFilterFun(FilterFun, Label, Mail, Data) ->
try FilterFun(Mail, Data) of
    Result -> {Label, Result}
catch
    _:_ -> {Label, unchanged}
end.

applyMerge(Label, AS, MergeFun, FunResults, MrgChain) ->
    Res = case MergeFun(FunResults) of 
            {just, UData} -> case MrgChain of
                                false -> io:format("code_change APPLY MERGE JUST 1 ~p~n", [[MrgChain, AS,Label]]),gen_server:cast(AS,{self(), Label, just, UData});
                                _ -> io:format("code_change APPLY MERGE CALL JUST 1 ~p~n", [[MrgChain, AS,Label]]),gen_server:reply(MrgChain, {Label,{just, UData}})       
                            end,
                            false;         
            {transformed, UMail} ->case MrgChain of
                                false -> io:format("code_change APPLY MERGE 1 TRANS~p~n", [[MrgChain, AS,Label]]),gen_server:cast(AS, {self(), Label, transformed, UMail});
                                _ -> io:format("code_change APPLY MERGE CALL TRANS 1 ~p~n", [[MrgChain, AS,Label]]),gen_server:reply(MrgChain, {transformed, UMail})
                            end,
                            false;
            unchanged -> case MrgChain of
                                false -> io:format("code_change APPLY MERGE 1 UNCH~p~n", [[MrgChain, AS,Label]]),gen_server:cast(AS, {self(), Label, unchanged});
                                _ -> io:format("code_change APPLY MERGE CALL UNCH 1 ~p~n", [[MrgChain, AS,Label]]),gen_server:reply(MrgChain, unchanged)
                            end,
                            false;                 
            {both, UMail, UData} -> case MrgChain of
                                    false -> io:format("code_change APPLY MERGE 1 BOTH ~p~n", [[MrgChain, AS,Label]]),gen_server:cast(AS, {self(), Label, both, UMail, UData});
                                    _ -> io:format("code_change APPLY MERGE CALL BOTH 1 ~p~n", [[MrgChain, AS,Label]]),gen_server:reply(MrgChain, {both, UMail, UData})
                                end,
                                false;
            continue -> MrgChain;
            _ -> MrgChain
        end,
    Res.

init(FilterData) ->
    
    %% FilterFun = {simple, filter_fun()} | {chain, [FilterFun]} | {group, [FilterFun], mergeFun}
    [Label, FilterFun, Mail, Data, MrgFun, AS] = FilterData, 
    {ok, #{label => Label, mail => Mail, data => Data, filter => FilterFun, as => AS, mergeFun => MrgFun, groupFilterServers => [], groupResults => #{}, mrgChain => false}}.



handle_call(get_filter, From, #{label := Label, mail := Mail, data := Data, filter := Filter}=State) ->
    case Filter of
        {simple, FilterFun} -> Result = runFilterFun(FilterFun, Label, Mail, Data),
                               {reply, Result, State};
        {chain, FilterFunList} -> Result = case lists:foldl(fun chainingHelper/2, {unchanged, Mail, Data, Label},FilterFunList) of
                                                {just, _, UData, _} -> {Label, {just, UData}};
                                                {transformed, UMail, _, _} -> {Label, {transformed, UMail}};
                                                {unchanged, _, _, _} -> {Label, unchanged};
                                                {both, UMail, UData, _} -> {Label, {both, UMail, UData}};
                                                _  -> io:format("FILTERFUN CALL NOT MATCH  ~p~n", [State]),{Label, unchanged}
                                            end,
                                            io:format("code_change GET_FILTER OLD STATE  ~p~n", [State]),
                                            io:format("code_change GET FILTER NEW STATE > MAIL > DATA > LABEL > FUNC - ~p >>>>> ~p~n", [FilterFunList, Result ]), 
                                {reply, Result, State};
        {group, FilterFunList, MergeFun} -> MFS = startNewFilters(Mail, Data, Label, none, FilterFunList),
                                            io:format("code_change RUN MERGE IN CHAIN FILTER MAIN: ~p~n", [State#{mergeFun := MergeFun, groupFilterServers := MFS, mrgChain := From}]),
                                            {noreply, State#{mergeFun := MergeFun, groupFilterServers := MFS, mrgChain := From}};
        {timelimit, TimeOut, FilterFun} -> {reply, {Label, unchanged}, State};
        _                   -> {reply, {Label, unchanged}, State}
        
                                
        % {chain, FilterFunList} -> try FilterFun(Mail, Data) of
        %                         {just, UData} -> gen_server:call(AS,{self(), Label, just, UData});
        %                         {transformed, UMail} -> gen_server:call(AS, {self(), Label, transformed, UMail});
        %                         unchanged -> gen_server:call(AS, {self(), Label, unchanged});
        %                         {both, UMail, UData} -> gen_server:call(AS, {self(), Label, both, UMail, UData})
        %                     catch
        %                         _:_ -> gen_server:call(AS, {self(), Label, unchanged})
        %                     end        
    end;
    
handle_call(_, _, State) ->
    {ok, State}.

updatedFilterState(Old, New) ->
    case {Old, New} of
        {S, unchanged} -> S;
        {unchanged, S} -> S;
        {_, both} -> both;
        {both, _} -> both;
        {S, S} -> S;
        {just, transformed} -> both;
        {transformed, just} -> both
    end.

chainingHelper(Filt, {S, Mail, Data, Label}) ->
    {ok, FSC} = start([Label, Filt, Mail, Data, none, self()]),
    
    Res = case gen_server:call(FSC, get_filter, infinity) of
            {L, {just, UData}} -> {updatedFilterState(S, just), Mail, UData, L};
            {L, {transformed, UMail}} -> {updatedFilterState(S, transformed), UMail, Data, L};
            {L, unchanged} -> {updatedFilterState(S, unchanged), Mail, Data, L};
            {L, {both, UMail, UData}} -> {updatedFilterState(S, both), UMail, UData, L}
        end,
    gen_server:cast(FSC, stop),
    io:format("code_change CHAINING HELPER OLD STATE  > MAIL > DATA > LABEL~p~n", [{Filt, S, Mail, Data, Label}]),
    io:format("code_change CHAINING HELPER NEW STATE > MAIL > DATA > LABEL > FUNC - ~p >>>>> ~p~n", [Filt, Res ]),
    Res.
    


handle_cast(run_filter, #{label := Label, mail := Mail, data := Data, filter := Filter, as := AS}=State) ->
    case Filter of
        {simple, FilterFun} -> try FilterFun(Mail, Data) of
                                    {just, UData} -> gen_server:cast(AS,{self(), Label, just, UData});
                                    {transformed, UMail} -> gen_server:cast(AS, {self(), Label, transformed, UMail});
                                    unchanged -> gen_server:cast(AS, {self(), Label, unchanged});
                                    {both, UMail, UData} -> gen_server:cast(AS, {self(), Label, both, UMail, UData})
                                catch
                                    _:_ -> gen_server:cast(AS, {self(), Label, unchanged})
                                end,
                                io:format("code_change RUN MERGE INNER FUNCTION: ~p ~n", [State]),
                                      
                                {noreply, State};
        {chain, FilterFunList} -> case lists:foldl(fun chainingHelper/2, {unchanged, Mail, Data, Label},FilterFunList) of
                                    {just, _, UData, _} -> gen_server:cast(AS,{self(), Label, just, UData});
                                    {transformed, UMail, _, _} -> gen_server:cast(AS, {self(), Label, transformed, UMail});
                                    {unchanged, _, _, _} -> gen_server:cast(AS, {self(), Label, unchanged});
                                    {both, UMail, UData, _} -> gen_server:cast(AS, {self(), Label, both, UMail, UData})
                                end,
                                {noreply, State};
        {group, FilterFunList, MergeFun} -> MFS = startNewFilters(Mail, Data, Label, none, FilterFunList),
                                    io:format("code_change RUN MERGE FILTER MAIN: ~p~n", [State#{mergeFun := MergeFun, groupFilterServers := MFS, mrgChain := false}]),
                                        {noreply, State#{mergeFun := MergeFun, groupFilterServers := MFS, mrgChain := false}};
        {timelimit, TimeOut, FilterFun} -> {noreply, State};
        _                           ->  {noreply, State}
   
    end;



handle_cast({MF, Label, just, UData}, #{groupResults := GroupResults, mergeFun := MergeFun, groupFilterServers := GroupFilterServers, as := AS, mrgChain := MrgChain}=State) ->
    UpdatedGroupResults = GroupResults#{MF => {just, UData}},
    io:format("code_change HANDLE INNER JUST MERGE RESULTS: ~p~n", [State#{groupResults => UpdatedGroupResults }]),
    Result = [ maps:get(F, UpdatedGroupResults, inprogress)|| F <- GroupFilterServers],
    UMrgChain = applyMerge(Label, AS, MergeFun, Result, MrgChain),

    {noreply, State#{groupResults => UpdatedGroupResults, mrgChain := UMrgChain}};

handle_cast({MF, Label, transformed, UMail}, #{groupResults := GroupResults, mergeFun := MergeFun, groupFilterServers := GroupFilterServers, as := AS, mrgChain := MrgChain}=State) ->
    UpdatedGroupResults = GroupResults#{MF => {transformed, UMail}},
    Result = [ maps:get(F, UpdatedGroupResults, inprogress)|| F <- GroupFilterServers],
    io:format("code_change HANDLE INNER TRANSFORMED MERGE RESULTS: ~p~n", [State#{groupResults => UpdatedGroupResults }]),
    UMrgChain = applyMerge(Label, AS, MergeFun, Result, MrgChain),

    {noreply, State#{groupResults => UpdatedGroupResults, mrgChain := UMrgChain}};

handle_cast({MF, Label, unchanged}, #{groupResults := GroupResults, mergeFun := MergeFun, groupFilterServers := GroupFilterServers, as := AS, mrgChain := MrgChain}=State) ->
    UpdatedGroupResults = GroupResults#{MF => unchanged},
    Result = [ maps:get(F, UpdatedGroupResults, inprogress)|| F <- GroupFilterServers],
    UMrgChain = applyMerge(Label, AS, MergeFun, Result, MrgChain),

    {noreply, State#{groupResults => UpdatedGroupResults, mrgChain := UMrgChain}};

handle_cast({MF, Label, both, UMail, UData}, #{groupResults := GroupResults, mergeFun := MergeFun, groupFilterServers := GroupFilterServers, as := AS, mrgChain := MrgChain}=State) ->
    UpdatedGroupResults = GroupResults#{MF => {both, UMail, UData}},
    Result = [ maps:get(F, UpdatedGroupResults, inprogress)|| F <- GroupFilterServers],
    UMrgChain = applyMerge(Label, AS, MergeFun, Result, MrgChain),

    {noreply, State#{groupResults => UpdatedGroupResults, mrgChain := UMrgChain}};

handle_cast(stop, #{groupFilterServers := GroupFilterServers}=State) ->
    stopAllFilters(GroupFilterServers),
    {stop, normal, State}.

handle_info(Info, State) ->
    io:format("Error: ~p~n",[Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    io:format("code_change: ~p~n", [Return]),
    Return.
-module(filterServer).
-behaviour(gen_server).

-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([runFilterFun/4, chainingHelper/2, updatedFilterState/2]).

start(FilterData) ->
    gen_server:start(?MODULE, FilterData, []).


runFilterFun(FilterFun, Label, Mail, Data) ->
try FilterFun(Mail, Data) of
    Result -> {Label, Result}
catch
    _:_ -> {Label, unchanged}
end.

init(FilterData) ->
    
    %% FilterFun = {simple, filter_fun()} | {chain, [{simple, filter_fun()}]}
   
    [Label, FilterFun, Mail, Data, AS] = FilterData,
    {ok, #{label => Label, mail => Mail, data => Data, filter => FilterFun, as => AS}}.



handle_call(get_filter, _, #{label := Label, mail := Mail, data := Data, filter := Filter}=State) ->
    case Filter of
        {simple, FilterFun} -> Result = runFilterFun(FilterFun, Label, Mail, Data),
                               {reply, Result, State};
        _ -> {reply, {Label, unchanged}, State}
                                
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
    {ok, FSC} = start([Label, Filt, Mail, Data, self()]),
    Res = case gen_server:call(FSC, get_filter) of
            {L, {just, UData}} -> {updatedFilterState(S, just), Mail, UData, L};
            {L, {transformed, UMail}} -> {updatedFilterState(S, transformed), UMail, Data, L};
            {L, unchanged} -> {updatedFilterState(S, unchanged), Mail, Data, L};
            {L, {both, UMail, UData}} -> {updatedFilterState(S, both), UMail, UData, L}
        end,
    gen_server:cast(FSC, stop),
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
                                end;
        {chain, FilterFunList} -> case lists:foldl(fun chainingHelper/2, {unchanged, Mail, Data, Label},FilterFunList) of
                                    {just, _, UData, _} -> gen_server:cast(AS,{self(), Label, just, UData});
                                    {transformed, UMail, _, _} -> gen_server:cast(AS, {self(), Label, transformed, UMail});
                                    {unchanged, _, _, _} -> gen_server:cast(AS, {self(), Label, unchanged});
                                    {both, UMail, UData, _} -> gen_server:cast(AS, {self(), Label, both, UMail, UData})
                            end
            
        % try FilterFun(Mail, Data) of
        %                         {just, UData} -> gen_server:cast(AS,{self(), Label, just, UData});
        %                         {transformed, UMail} -> gen_server:cast(AS, {self(), Label, transformed, UMail});
        %                         unchanged -> gen_server:cast(AS, {self(), Label, unchanged});
        %                         {both, UMail, UData} -> gen_server:cast(AS, {self(), Label, both, UMail, UData})
        %                     catch
        %                         _:_ -> gen_server:cast(AS, {self(), Label, unchanged})
        %                     end        
    end,
    {noreply, State};

handle_cast({_, Label, just, UData}, #{filterResults := FilterResults}=State) ->
    {noreply, State#{filterResults => FilterResults#{Label => {done, UData} }}};

% handle_cast({F, Label, transformed, UMail}, #{filterData := FilterD, filterServers := FilterServers}=State) ->
%     #{Label := {_, Data}}=FilterD,
%     stopAllFilters(lists:delete(F, FilterServers)),
%     UpdatedServers = startFilters(UMail, maps:remove(Label, FilterD)),
%     UpdatedState = State#{ mail => UMail, filterServers => UpdatedServers, filterresults => #{Label => {done, Data}}},
%     {noreply, UpdatedState};

% handle_cast({_, Label, unchanged}, #{filterData := FilterD, filterResults := FilterResults}=State) ->
%     #{Label := {_, Data}}=FilterD,
%     {noreply, State#{filterResults => FilterResults#{Label => {done, Data} }}};

% handle_cast({F, Label, both, UMail, UData}, #{filterData := FilterD, filterServers := FilterServers}=State) ->
%     stopAllFilters(lists:delete(F, FilterServers)),
%     UpdatedServers = startFilters(UMail, maps:remove(Label, FilterD)),
%     UpdatedState = State#{ mail => UMail, filterServers => UpdatedServers, filterresults => #{Label => {done, UData}}},
%     {noreply, UpdatedState};

handle_cast(stop, State) ->
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
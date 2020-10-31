-module(filterServer).
-behaviour(gen_server).

-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start(FilterData) ->
    gen_server:start(?MODULE, FilterData, []).



init(FilterData) ->
    [Label, FilterFun, Mail, Data, AS] = FilterData,
    {ok, #{label => Label, mail => Mail, data => Data, filter => FilterFun, as => AS}}.

handle_call(_, _, State) ->
    {ok, State}.

handle_cast(runSimple, #{label := Label, mail := Mail, data := Data, filter := Filter, as := AS}=State) ->
    case Filter of
        {simple, FilterFun} -> try FilterFun(Mail, Data) of
                                    {just, UData} -> gen_server:cast(AS,{self(), Label, just, UData});
                                    {transformed, UMail} -> gen_server:cast(AS, {self(), Label, transformed, UMail});
                                    unchanged -> gen_server:cast(AS, {self(), Label, unchanged});
                                    {both, UMail, UData} -> gen_server:cast(AS, {self(), Label, both, UMail, UData})
                                catch
                                    _:_ -> gen_server:cast(AS, {self(), Label, unchanged})
                                end
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
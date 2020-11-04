-module(mailServer).
-behaviour(gen_server).
 -import(analysisServer, [start_link/1]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([stopFiltersAndGetAnalysis/1, stopAnalysis/1,stopAllAnalysis/1]).

start_link() ->
    gen_server:start(?MODULE, [], []).

stopFiltersAndGetAnalysis(A) -> 
    gen_server:call(A, stop_filters, infinity).

stopAnalysis(A) ->
    gen_server:cast(A, stop).

stopAllAnalysis(AL) ->
    lists:foreach(fun stopAnalysis/1, AL).


init(_Args) ->
    %% filters labels => {filt, Data} 
    %% filt = {simple, filter_fun()} | {chain, [{simple, filter_fun()}]}
    {ok, #{capacity => infinite, mails => [], filters => #{}}}. 

handle_call(stop, _, #{mails := MailsL}=State) ->
    
    Result = [ stopFiltersAndGetAnalysis(M) || M <- MailsL],
    stopAllAnalysis(MailsL),
    % io:format("STOP MAILSERVER: -----------------------------------------------------~p~n", [MailsL]),
    {reply, {ok, Result}, State#{mails := [], filters := #{}}};


    
handle_call({add_mail, Mail}, _, #{mails := MailsL, filters := FiltersL}=State) ->
    {ok, MAnalysis} =  analysisServer:start_link([Mail, FiltersL, self()]),
    UpdatedMails = MailsL ++ [MAnalysis],
    {reply, {ok, MAnalysis}, State#{mails := UpdatedMails}}.


handle_cast({default, Label, Filt, Data}, #{filters := Filters}=State) ->
    case maps:get(Label, Filters, none) of
        none -> case Filt of
                    {simple, _} -> UpdatedFilters = Filters#{Label => {Filt, Data}},
                                UpdatedState = State#{filters := UpdatedFilters},
                                {noreply, UpdatedState};
                    {chain, _} -> UpdatedFilters = Filters#{Label => {Filt, Data}},
                                UpdatedState = State#{filters := UpdatedFilters},
                                {noreply, UpdatedState};
                    {group, _, _} -> UpdatedFilters = Filters#{Label => {Filt, Data}},
                                UpdatedState = State#{filters := UpdatedFilters},
                                {noreply, UpdatedState};
                    {timelimit, _, _} -> UpdatedFilters = Filters#{Label => {Filt, Data}},
                                UpdatedState = State#{filters := UpdatedFilters},
                                {noreply, UpdatedState};
                    _ ->  %io:format("code_change MAILSERVER_DEFAULT_NONE: ~p~n", [State]),
                        {noreply, State}
                end;
        _ ->  %io:format("code_change MAIL_SERVER_DEFAULT: ~p~n", [State]),
                {noreply, State}

    end;

handle_cast({remove_analysis, A}, #{mails := MailsL}=State) ->
    stopAnalysis(A),
    % io:format("REMOVE ANALYSIS FROM MAILSERVER: ~p~n", [lists:delete(A, MailsL)]),
    
    {noreply, State#{mails := lists:delete(A, MailsL)}};

handle_cast(stop, #{mails := MailsL}=State) ->
    stopAllAnalysis(MailsL),
    {stop, normal, State}.


handle_info(Info, State) ->
    % io:format("Error: ~p~n",[Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    io:format("code_change: ~p~n", [Return]),
    Return.
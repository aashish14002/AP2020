-module(test_qc_mailfilter).

-include_lib("eqc/include/eqc.hrl").
-include("apqc_statem.hrl").

-behaviour(apqc_statem).
-export([initial_state/0, command/1, precondition/2, postcondition/3, next_state/3]).

-export([prop_mailfilter/0,prop_mail_is_sacred/0]).
-export([add_mail_to/2,start_mail_server/1, add_default_filter_to/4,
            get_mail_config/1, stop_mail_analysis/1, add_filter_to/4]). % Remember to export the other function from Q2.2

%  from quickcheck finale reg.erl from the course material
prop_mailfilter() ->
  ?FORALL(Cmds,commands(?MODULE),
     begin
        eqc:format("~p~n------------------------------------------------------",[Cmds]),
         {_,S,_} = Result = run_commands(?MODULE, Cmds),
         cleanup(S),
         check_commands(Cmds, Result, true)
     end).

prop_mail_is_sacred() ->
    ?FORALL(Cmds,commands(?MODULE),
        begin
            % eqc:format("~p~n------------------------------------------------------",[Cmds]),
            {_,S,_} = Result = run_commands(?MODULE, Cmds),
            AA=case maps:get(mail_serv, S) of 
                none -> true;
                A -> {ok, Res} = mailfilter:stop(A),
                            model_check_mailfilter_config(S, Res)
            end,
            case AA of
                false -> eqc:format("~p~n------------------------------------------------------",[Cmds]),1;
                true -> none
            end,
            % ML = mailfilter:stop(maps:get(mail_serv, S)),
            eqc:format("~p~n------------------------------------------------------",[AA]),
            
            check_commands(Cmds, Result, AA)
            
        end).



check_commands(Cmds, {_,_,Res} = HSRes, AA) ->
    pretty_commands(?MODULE, Cmds, HSRes,
                    aggregate(command_names(Cmds),
                              AA)).



% Cleanup, unregister all names (potentially) registered during test,
% and kill all processes stated.
cleanup(#{mail_serv := none})->
    ok;
cleanup(#{mail_serv := A})->
    mailfilter:stop(A).
    % [catch erlang:unregister(N) || N<-names()],
    % [exit(P,kill) || P <- maps:get(pids, S)].
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
any_data()->
    bitstring().

mail() ->
    bitstring().

filter_label() ->
    choose(1,10).

unchanged() ->
    unchanged.

filter_result() ->
    oneof([
            ?LET(D,any_data(),{just, D}), 
            ?LET(M,mail(),{transformed, M}),
            ?LET({M,D},any_data(),{both, M, D}),
            unchanged()
        ]).

wellbehaved_filter_fun() ->
    oneof([
        change_data_filter_fun(),
        change_mail_filter_fun(),
        change_both_filter_fun(),
        change_nothing_filter_fun()
    ]).

change_data_filter_fun() ->
    fun(_, _) -> {just, n} end.

change_mail_filter_fun() ->
    (fun(_, _) -> {transformed, n} end).

change_both_filter_fun() ->
    (fun(_, _) -> {both, n, n} end).

change_nothing_filter_fun() ->
    (fun(_, _) -> unchanged end).


timelimit() ->
    choose(1,5000).

infinite() ->
    infinity.

timeout() ->
    oneof( [timelimit(), infinite()]).

inProgress(P) ->
    case P of
        inprogress -> true;
        _ -> false
    end.
    
merge_fun() ->
    fun(F) -> 
        case lists:any(fun inProgress/1,F) of
            true -> continue;
            _  -> elements(F)
        end
    end.



filter(FunGen) ->
    oneof([ 
        ?LET(Fun,FunGen,{simple, Fun}),
        ?LET(Fun,list(FunGen),{chain, Fun}),
        ?LET({Fun, MergeFun},{list(FunGen), merge_fun()},{group, Fun, MergeFun }),
        ?LET(Fun,list(FunGen),{timelimit, timeout(), Fun})
    ]).

wellbehaved_filter() ->
    filter(wellbehaved_filter_fun()).

get_mail_analysis(ML) ->
    elements(ML). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Models
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
model_add_mail(Mail, MailList) ->
    [Mail|MailList].

model_stop_mail_analysis(M, ML) ->
    lists:delete(M, ML).

model_add_default_filter(L, F, D, DF) ->
    case maps:get(L, DF, none) of
        none -> DF#{L => {F, D}};
        _ -> DF
    end.
% list({mail(), list({label(), result()})})
model_check_mailfilter_config(#{mails := ML}, Res) ->
    % A = change_data_filter_fun(),
    % B = A(1,2),
    % eqc:format("~p~n------------------------------------------------------",[B]),
    (length(ML) =:= length(Res)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calls to mailfilter server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_mail_server(C) ->
    {_, MS} = mailfilter:start(C),
    MS.

add_mail_to(A, M) ->
    {_, MR} = mailfilter:add_mail(A, M),
    MR.

add_default_filter_to(MS, Label, Filt, Data) ->
    mailfilter:default(MS, Label, Filt, Data),
    ok.

get_mail_config(MR) ->
    {_, C} = mailfilter:get_config(MR),
    C.

stop_mail_analysis(MR) ->
    try mailfilter:enough(MR) of
        _ -> true
    catch
        _:_ -> true
    end,
    ok.

add_filter_to(MR, Label, Filt, Data) ->
    mailfilter:add_filter(MR, Label, Filt, Data),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Initial state
%%  mail_serv : pid     mailfilter server id
%%  mails : [pid]       list of mail analysis server ids
%%  filters: #{filter_label() := {filter(), any_data()}} map of default filters 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initial_state() ->
    #{mail_serv => none, mails => [], filters => #{}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
command(#{mail_serv := none}) ->
    return({call, ?MODULE, start_mail_server, [timeout()]});

% command(#{ mails := [], mail_serv := A}) ->
%     oneof([
%             {call, ?MODULE, add_mail_to, [A, mail()]}
%             % {call, ?MODULE, add_default_filter_to, 
%             %     [A, filter_label(), wellbehaved_filter(), any_data()]}
%         ]);

command(#{mails := ML, mail_serv := A}) ->
    oneof([
            {call, ?MODULE, add_mail_to, [A, mail()]},
            {call, ?MODULE, add_default_filter_to, 
                [A, filter_label(), wellbehaved_filter(), any_data()]}
            ]++ 
            % {call, ?MODULE, add_filter_to, [get_mail_analysis(ML), filter_label(),
            %      wellbehaved_filter(), any_data()]},
            % {call, ?MODULE, get_mail_config, [get_mail_analysis(ML)]},
            [{call, ?MODULE, stop_mail_analysis, [get_mail_analysis(ML)]} || ML =/= [] 
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% State Transitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
next_state(S, V, {call, _, start_mail_server, _}) ->
    S#{mail_serv := V};

next_state(#{mails := ML}=S, V, {call, ?MODULE, add_mail_to, [_, _]}) ->
    S#{mails := model_add_mail(V, ML)};

next_state(#{mails := ML}=S, _, {call, ?MODULE, stop_mail_analysis, [M]}) ->
    S#{mails := model_stop_mail_analysis(M, ML)};
    
next_state(#{filters := DF}=S, _, 
        {call, ?MODULE, add_default_filter_to, [_, L, F, D]}) ->
    S#{filters := model_add_default_filter( L, F, D, DF)};
% next_state(_S, _, {call, mailfilter, stop, [_A]}) ->
%     #{mail_serv => none, mails => []};

next_state(S, _, _) ->
    S.
% precondition(#{mails := ML}, {call, _, stop_mail_analysis, [_]}) ->
%     if (ML =:= []) -> false; true -> true end;

% precondition(#{mail_serv := MS}, {call, _, stop, [_]}) ->
%     if (MS =:= none) -> false; true -> true end;

% precondition(#{mail_serv := MS}, {call, _, start, [_]}) ->
%     if (MS =:= none) -> true; true -> false end;

precondition(_S, _) ->
    true.

postcondition(_S, _, _Res) ->
    true.

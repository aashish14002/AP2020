-module(test_qc_mailfilter).

-include_lib("eqc/include/eqc.hrl").
-include("apqc_statem.hrl").

-behaviour(apqc_statem).
-export([initial_state/0, command/1, precondition/2, postcondition/3, next_state/3]).

-export([prop_mailfilter/0,add_mail_to/2,start_mail_server/1]). % Remember to export the other function from Q2.2

%  from quickcheck finale reg.erl from the course material
prop_mailfilter() ->
  ?FORALL(Cmds,commands(?MODULE),
     begin
        eqc:format("~p~n------------------------------------------------------",[Cmds]),
         {_,S,_} = Result = run_commands(?MODULE, Cmds),
         cleanup(S),
         check_commands(Cmds, Result)
     end).

check_commands(Cmds, {_,_,Res} = HSRes) ->
    pretty_commands(?MODULE, Cmds, HSRes,
                    aggregate(command_names(Cmds),
                              equals(Res, ok))).

% Cleanup, unregister all names (potentially) registered during test,
% and kill all processes stated.
cleanup(#{mail_serv := none})->
    ok;
cleanup(#{mail_serv := A})->
    mailfilter:stop(A).
    % [catch erlang:unregister(N) || N<-names()],
    % [exit(P,kill) || P <- maps:get(pids, S)].
    

%%%% Generators
any_data()->
  elements([1,2,3,4]).

mail() ->
  elements([1,2,3,4]).

just() ->
  {just, any_data()}.

transformed() ->
  {transformed, mail()}.

unchanged() ->
  unchanged.

both() ->
  {both, mail(), any_data()}.

filter_result() ->
  oneof([just(), transformed(), unchanged(), both()]).

wellbehaved_filter_fun() ->
  F = fun(_, _) -> filter_result() end,
  F.

infinite() ->
  infinite.

timelimit() ->
    choose(1,5000).

timeout() ->
  oneof( [timelimit(), infinite()]).

continue() ->
  continue.

merge_fun() ->
  F = fun(_, _) -> oneof([filter_result(),continue()]) end,
  F.

simple_filter(FunGen) ->
  {simple, FunGen}.

chain_filter(FunGen) ->
  {chain, list(FunGen)}.

group_filter(FunGen) ->
  {group, list(FunGen), merge_fun() }.

timelimit_filter(FunGen) ->
  {timelimit, timeout(), FunGen}.


filter(FunGen) ->
  oneof([ simple_filter(FunGen), chain_filter(FunGen), group_filter(FunGen), timelimit_filter(FunGen)]).


model_add_mail(Mail, MailList) ->
    case lists:member(Mail, MailList) of 
        true -> MailList;
        false -> [Mail|MailList]
    end.

start_mail_server(C) ->
    {ok, MS} = mailfilter:start(C),
    MS.

add_mail_to(A, M) ->
    mailfilter:add_mail(A, M),
    ok.


initial_state() ->
  #{mail_serv => none, mails => []}.

command(#{mail_serv := none}) ->
  return({call, ?MODULE, start_mail_server, [timeout()]});

command(#{mail_serv := A}) ->
  oneof([
        {call, ?MODULE, add_mail_to, [A, mail()]},    
        {call, mailfilter, stop, [A]}
    ]).


next_state(S, V, {call, _, start_mail_server, _}) ->
  S#{mail_serv := V};

next_state(#{mails := ML}=S, _, {call, ?MODULE, add_mail_to, [_, M]}) ->
  S#{mails := model_add_mail(M, ML)};

next_state(_S, _, {call, mailfilter, stop, [_A]}) ->
    #{mail_serv => none, mails => []};

next_state(S, _, _) ->
  S.
precondition(#{mail_serv := MS}, {call, _, add_mail_to, [_, _]}) ->
    if (MS =:= none) -> false; true -> true end;

precondition(#{mail_serv := MS}, {call, _, stop, [_]}) ->
    if (MS =:= none) -> false; true -> true end;

% precondition(#{mail_serv := MS}, {call, _, start, [_]}) ->
%     if (MS =:= none) -> true; true -> false end;

precondition(_S, _) ->
    true.

postcondition(_S, _, _Res) ->
    true.

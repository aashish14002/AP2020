-module(mailfilter).

% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called mailfilter.

-import(mailServer, [start_link/0]).
% Export at least the API:
-export(
  [ start/1
  , stop/1
  , default/4
  , add_mail/2
  , get_config/1
  , enough/1
  , add_filter/4
  ]).

% You may have other exports as well
-export([]).

% API :

start(_Cap) ->
  mailServer: start_link().

stop(_MS) ->
  State = gen_server:call(_MS, stop),
  gen_server:cast(_MS, stop),
  State.

add_mail(_MS, _Mail) ->
  gen_server:call(_MS, {add_mail, _Mail}).

get_config(_MR) ->
  gen_server:call(_MR, get_config).

default(_MS, _Label, _Filt, _Data) ->
  gen_server:cast(_MS, {default, _Label, _Filt, _Data}).

enough(_MR) ->
  gen_server:cast(_MR, enough).

add_filter(_MR, _Label, _Filt, _Data) ->
  gen_server:cast(_MR, {add_filter, _Label, _Filt, _Data}).

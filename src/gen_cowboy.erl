-module(gen_cowboy).
-export([start_http/1, start_http/2]).
-export([start_https/1, start_https/2]).
-export([start_spdy/1, start_spdy/2]).
-export([start/2, start/3]).
-export([reload/1]).

start_http(Module) ->
  start_http(Module, []).
start_http(Module, Opts) ->
  start(http, Module, Opts).

start_https(Module) ->
  start_https(Module, []).
start_https(Module, Opts) ->
  start(https, Module, Opts).

start_spdy(Module) ->
  start_spdy(Module, []).
start_spdy(Module, Opts) ->
  start(spdy, Module, Opts).

start(Protocol, Module) ->
  start(Protocol, Module, []).
start(P, Module, Opts) when P =:= http orelse P =:= https orelse P =:= spdy ->
  Fun = list_to_atom("start_" ++ atom_to_list(P)),
  start(Fun, Module, Opts);
start(Protocol, Module, Opts) ->
  %% force load the module
  Module:module_info(),
  State = case exec(Module, init, Opts, {ok, undefined}) of
    {ok, S} -> S;
    S -> S
  end,
  cowboy:Protocol(Module, size(Module, State), listener_opts(Module, State), protocol_opts(Module, State)).

reload(Module) ->
  Opts = ranch:get_protocol_options(Module),
  {_, Val} = lists:keyfind(gen_cowboy_opts, 1, Opts),
  ok = ranch:set_protocol_options(Module, protocol_opts(Module, Val)).

size(Module, Opts) ->
  exec(Module, size, Opts, 100).

listener_opts(Module, Opts) ->
  AdditionalOpts = [
    {port, simple_env:get_integer("PORT", 8080)}
  ],

  exec(Module, listener_opts, Opts, [])++
    additional_opts(Module, Opts, AdditionalOpts, []).

protocol_opts(Module, Opts) ->
  AdditionalOpts = [
    {compress, true},
    {env, []},
    max_empty_lines,
    max_header_name_length,
    max_header_value_length,
    max_headers,
    max_keepalive,
    max_request_line_length,
    onresponse,
    timeout
  ],

  exec(Module, protocol_opts, Opts, []) ++
    [{gen_cowboy_opts, Opts}] ++
    middlewares(Module, Opts) ++
    additional_opts(Module, Opts, AdditionalOpts, []).

middlewares(Module, Opts) ->
  case exec(Module, middlewares, Opts, undefined) of
    undefined ->
      [];
    Middleware ->
      [{middlewares, [cowboy_estack:init(Middleware)]}]
  end.

additional_opts(_, _, [], Acc) ->
  Acc;
additional_opts(Module, Opts, [Fun|Rest], Acc) when is_atom(Fun) ->
  additional_opts(Module, Opts, [{Fun, undefined}|Rest], Acc);
additional_opts(Module, Opts, [{Fun, Default}|Rest], Acc) ->
  Conf = case exec(Module, Fun, Opts, Default) of
    undefined ->
      Acc;
    Val ->
      [{Fun, Val}|Acc]
  end,
  additional_opts(Module, Opts, Rest, Conf).

exec(Module, Fun, Opts, Default) ->
  case erlang:function_exported(Module, Fun, 0) of
    false ->
      case erlang:function_exported(Module, Fun, 1) of
        false ->
          Default;
        true ->
          Module:Fun(Opts)
      end;
    true ->
      Module:Fun()
  end.

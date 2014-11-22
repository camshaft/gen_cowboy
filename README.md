gen_cowboy
==========

generic cowboy server with some nice defaults

Usage
-----

Start the server with `start_http/1`, `start_https/1` or `start_spdy/1`:

```erlang
gen_cowboy:start_http(my_server).
```

Provide the callbacks:

```erlang
-module(my_server).

-export([init/1]).
-export([port/0]).
-export([middlewares/1]).

init(Opts) ->
  {ok, Opts}.

port(_State) ->
  5000.

middlewares(_State) ->
  [
    cowboy_router,
    cowboy_handler
  ].
```

The valid callbacks are listed in the [cowboy_protocol docs](http://ninenines.eu/docs/en/cowboy/HEAD/manual/cowboy_protocol/).

A `reload/1` function is provided to reload any protocol changes:

```erlang
gen_cowboy:reload(my_server).
```

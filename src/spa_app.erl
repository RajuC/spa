-module(spa_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
%%    PrivDir = code:priv_dir(ac),
%%    io:format("PrivDir:~p~n",[PrivDir]),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, spa, "index.html"}},
            {"/wifi", spa_http_handler, []},
            {"/user", spa_user_handler, []},
            {"/[...]", cowboy_static, {priv_dir, spa, ""
                }}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100,
        [{port, 8001}],
        [{env, [{dispatch, Dispatch}]}
    ]),
    spa_sup:start_link().

stop(_State) ->
	ok.

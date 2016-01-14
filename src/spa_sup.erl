-module(spa_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	io:format("~nsup~n"),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	ChildSpecList = 
		[{spa_main_service,{spa_main_service,start_link,[]},permanent,5000,worker,[spa_main_service]},
		{spa_mnesia_service,{spa_mnesia_service,start_link,[]},permanent,5000,worker,[spa_mnesia_service]},
		{spa_mysql_service,{spa_mysql_service,start_link,[]},permanent,5000,worker,[spa_mysql_service]}],
	{ok,{{one_for_one,30,60},ChildSpecList}}.
-module(spa_utils).
-export([start/0,to_list/1,to_bin/1]).



start() ->
	application:start(crypto),
	application:start(cowlib),
	application:start(ranch),
	application:start(cowboy),
	application:start(spa).

to_list({A,B})  ->
	{to_list(A),to_list(B)};
to_list({A,B,C}) ->
	{to_list(A),to_list(B),to_list(C)};
to_list(Data) when is_list(Data) ->
	Data;
to_list(Data) when is_atom(Data) ->
	atom_to_list(Data);
to_list(Data) when is_integer(Data) ->
	integer_to_list(Data);
to_list(Data) when is_binary(Data) ->
	binary_to_list(Data).


to_bin({A,B})  ->
	{to_bin(A),to_bin(B)};
to_bin({A,B,C}) ->
	{to_bin(A),to_bin(B),to_bin(C)};
to_bin(A) when is_list(A) ->
	list_to_binary(A);
to_bin(A) when is_integer(A) ->
	integer_to_binary(A);
to_bin(A) when is_atom(A) ->
	list_to_binary(to_list(A));
to_bin(A) when is_binary(A) ->
	A.
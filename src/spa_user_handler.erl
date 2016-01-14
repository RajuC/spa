-module(spa_user_handler).
-export([init/2]).

init(Req, Opts) ->
	io:format("Spa_User_Handler Req: ~p~n",[Req]),
	Method = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req),
	Req2 = handle_req(Method, HasBody, Req),
	{ok, Req2, Opts}.

handle_req(<<"POST">>, true, Req) ->
	{ok, PostVals, Req2} = cowboy_req:body_qs(Req),
	io:format("Post values user:: ~p~n",[PostVals]),
	Action = proplists:get_value(<<"action">>, PostVals),
	case Action of
		<<"add_user">> ->
			add_user(PostVals,Req2);
		<<"delete_user">> ->
			delete_user(PostVals,Req2)
	end;

handle_req(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
handle_req(_, _, Req) ->
	cowboy_req:reply(405, Req).



add_user(PostVals,Req) ->
	Name = proplists:get_value(<<"name">>, PostVals),
	Age = proplists:get_value(<<"age">>, PostVals),
	Gender = proplists:get_value(<<"gender">>, PostVals),
	Phone = proplists:get_value(<<"phone">>, PostVals),
	Address = proplists:get_value(<<"address">>, PostVals),
	Occu = proplists:get_value(<<"occupation">>, PostVals),
	case spa_main_service:add_user(to_list(Name),to_list(Age),
			to_list(Gender),to_list(Phone),
			to_list(Address),to_list(Occu)) of
		{ok,Response} ->
			cowboy_req:reply(200, [
				{<<"content-type">>, <<"text/plain; charset=utf-8">>}
					], to_bin(Response), Req);
		{error,Reason} ->
			cowboy_req:reply(400, [],Reason, Req)
	end.


delete_user(PostVals,Req) ->
	UserName = proplists:get_value(<<"name">>, PostVals),
	case spa_main_service:delete_user(to_list(UserName)) of
		{ok,Response} ->
			cowboy_req:reply(200, [
				{<<"content-type">>, <<"text/plain; charset=utf-8">>}
					], to_bin(Response), Req);
		{error,Reason} ->
			cowboy_req:reply(400, [],Reason, Req)
	end.

to_list(A) ->
	spa_utils:to_list(A).	

to_bin(A) ->
	spa_utils:to_bin(A).
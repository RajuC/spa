-module(spa_http_handler).
-export([init/2]).

init(Req, Opts) ->
	io:format("Spa_http_Handler Req: ~p~n",[Req]),
	Method = cowboy_req:method(Req),
%%	HasBody = cowboy_req:has_body(Req),
	Req2 = handle_req(Method, Req),
	{ok, Req2, Opts}.

handle_req(<<"POST">>, Req) ->
	{ok, PostVals, Req2} = cowboy_req:body_qs(Req),
	io:format("PostVals: ~p~n",[PostVals]),	
	TagReader = proplists:get_value(<<"tag_reader_id">>, PostVals),
	Tag = proplists:get_value(<<"tag_id">>, PostVals),
	io:format("TagId:~p~nTagREader: ~p~n",[Tag,TagReader]),
	case spa_main_service:process_req(to_list(TagReader),to_list(Tag)) of
		{ok,Response} ->
			io:format("Response ~p~n",[Response]),
			cowboy_req:reply(200, [
				{<<"content-type">>, <<"text/plain">>}
					], to_bin(Response), Req2);
		{error,Reason} ->
			io:format("Error Reason ~p~n",[Reason]),
			cowboy_req:reply(400, [],Reason, Req2)
	end;
%%handle_req(<<"POST">>, Req) ->
%%	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
handle_req(<<"GET">>, Req) ->
	#{tag_id := TagId} =cowboy_req:match_qs([tag_id], Req),
	#{tag_reader_id := TagReaderId}=cowboy_req:match_qs([tag_reader_id], Req),
%%	TagId=cowboy_req:qs_val(<<"tag_id">>, Req),
%%	TagReaderId=cowboy_req:qs_val(<<"tag_id">>, Req),
	io:format("TagId:~p~nTagREader: ~p~n",[TagId,TagReaderId]),
	cowboy_req:reply(405, Req).

to_list(A) ->
	spa_utils:to_list(A).	

to_bin(A) ->
	spa_utils:to_bin(A).	

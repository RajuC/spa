%% @doc this service is used for processing the requests and forwarding the data to the
%% database services.
%% this service also add the user's data to the database. 

-module(spa_main_service).
%%-export([process_req/2]).
-behaviour(gen_server).
-export([start_link/0,init/1,stop/0]).
-export([handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([process_req/2,add_user/6,delete_all_tables/0,delete_user/1,add_location/0]).
-include("spa.hrl").
-define(LOC,["Diary","Cosmetics","Electronics","MOBILES"]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE,{stop}).

init([]) ->
	io:format("~nSpa main service startrting ...~n"),
	{ok,[]}.

add_location() ->
	gen_server:call(?MODULE,{add_loc}).

process_req(TagReaderId,TagId) ->
	gen_server:call(?MODULE,{process_request,TagReaderId,TagId}).

add_user(Name,Age,Gender,Phone,Address,Occuptn) ->
	gen_server:call(?MODULE,{add_user,Name,Age,Gender,Phone,Address,Occuptn}).

delete_user(UserName) ->
	gen_server:call(?MODULE,{delete_user,UserName}).

delete_all_tables() ->
	gen_server:call(?MODULE,{delete_tables}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({add_loc},_From,State) ->
	Reply = add_loc_info(?LOCATION),
	{reply,Reply,State};
 
handle_call({delete_user,UserName},_From,State) ->
	Reply = delete_user_info(UserName),
	{reply,Reply,State};

handle_call({delete_tables},_From,State) ->
	Reply = delete_tables(),
	{reply,Reply,State};

handle_call({add_user,Name,Age,Gender,Phone,Address,Occuptn},_From,State) ->
	Reply = add_user_info(Name,Age,Gender,Phone,Address,Occuptn),
	{reply,Reply,State};

handle_call({process_request,TagReaderId,TagId},_From,State) ->
	process_request(TagReaderId,TagId),
	{reply,{ok,"processed_request"},State};

handle_call(_Request,_From,State) ->
	{reply,{error,notImplemented},State}.

handle_cast({stop},State) ->
	{stop,normal,State}.

handle_info(_Info,State) ->
	{noreply,State}.

terminate(_Reason,_State) ->
	ok.

code_change(_OldVsn,State,_Extra) ->
	{ok,State}.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_loc_info([]) ->
	{ok,"added_location"};
add_loc_info([{LocId,LocName}|Rest]) ->
	spa_mysql_service:add_location(LocId,LocName),
	add_loc_info(Rest).
	
delete_user_info(UserName) ->
	case spa_mnesia_service:delete_user(UserName) of
		{ok,[]} ->
			{ok,"Error : Name not found!!"};
		{ok,TagId} ->
			spa_mysql_service:delete_user(UserName),
			spa_mnesia_service:update_tag(TagId,"No"),
			{ok,"deleted successfully"};
		{error,_Reason} ->
			{error,"Internal error"}
	end.


delete_tables() ->
	spa_mnesia_service:delete_all_tables(),
	spa_mysql_service:delete_all_tables(),
	ok.

add_user_info(Name,Age,Gender,Phone,Address,Occuptn) ->
	{TagNum,TagId} = get_tag_id(),
	spa_mnesia_service:add_user(TagNum,Name,Age,Gender,Phone,Address,Occuptn,"Active"),
	spa_mysql_service:add_user(TagNum,Name,Age,Gender,Phone,Address,Occuptn,"Active"),
	spa_mnesia_service:update_tag(TagId),
	{ok,TagId}.

process_request(TagReaderId,TagId) ->
	case add_tag_reader_ext(TagReaderId) of
		{error,Reason} ->
			{error,Reason};
		{ok,TagReaderInfoRec} ->
			case add_tag_ext(TagId) of
				{error,Reason} ->
					{error,Reason};
				{ok,TagInfoRec} ->
					case  TagInfoRec#tags_info.assigned of
						"No" ->
							{ok,added};
						"Yes"->	
							TagNum = TagInfoRec#tags_info.tag_num,
							TagReaderNum =TagReaderInfoRec#tag_reader_info.tag_reader_num,
%%							[UserInfoRec] = mnesia:dirty_read(users_info,TagId),
							spa_mnesia_service:track_user_info(TagNum,TagReaderNum),
							spa_mysql_service:track_user_info(TagNum,TagReaderNum),
							{ok,"tracked"}
					end
			end
	end.
						




add_tag_ext(TagId) ->		
	case catch  mnesia:dirty_read(tags_info,TagId) of
		{'EXIT' ,Reason} ->
			io:format("EXIT_Error2: ~p",[Reason]), 
			{error,Reason};
		[] ->
			io:format("Adding Tag Id Info to the Database. "),
			TagNum = spa_mysql_service:ran_num("tag"),	
			spa_mnesia_service:add_tag(TagNum,TagId),
			spa_mysql_service:add_tag(TagNum,TagId),
			[TagInfoNewRec]=mnesia:dirty_read(tags_info,TagId),
			{ok,TagInfoNewRec}; 
		[TagInfoRec] ->
			{ok,TagInfoRec}
	end.


add_tag_reader_ext(TagReaderId) ->
	case catch  mnesia:dirty_read(tag_reader_info,TagReaderId) of
		{'EXIT' ,Reason} ->
			io:format("EXIT_Error1: ~p",[Reason]),
			{error,Reason};
		[] ->
			io:format("Adding Tag Reader Info to the Database"),
			LocationId = get_location(),
			ReaderNum = spa_mysql_service:ran_num("reader"),
			spa_mnesia_service:add_tag_reader(ReaderNum,TagReaderId,LocationId),
			spa_mysql_service:add_tag_reader(ReaderNum,TagReaderId,LocationId),
			[TagReaderInfoNewRec]= mnesia:dirty_read(tag_reader_info,TagReaderId),
			{ok,TagReaderInfoNewRec};
		[TagReaderInfoRec] ->
			{ok,TagReaderInfoRec}
	end.

get_tag_id() ->
	case catch mnesia:dirty_all_keys(tags_info) of
		{'EXIT' ,Reason} ->
			io:format("EXIT_Error1: ~p",[Reason]),
			{error,Reason};
		[] ->
			{error,no_tag_found};
		TagIdsList ->
			get_unassigned_tag(TagIdsList)
	end.

get_unassigned_tag(TagIdsList)	->
	get_unassigned_tag(TagIdsList,[]).
get_unassigned_tag([],TagIdsList) ->
	TagId = hd(lists:reverse(TagIdsList)),
	case catch mnesia:dirty_read(tags_info,TagId) of
		{'EXIT',Reason} ->
			{error,Reason};
		[] -> 
			{ok,[]};
		[TagRec] -> 
			TagNum= TagRec#tags_info.tag_num,
			{TagNum,TagId}
	end;
get_unassigned_tag([TagId|Rest],Result) ->
	[TagIdsRec] = mnesia:dirty_read(tags_info,TagId),
	Assigned = TagIdsRec#tags_info.assigned,
	case Assigned of
		"Yes" ->
			get_unassigned_tag(Rest,Result);
		"No" ->
			get_unassigned_tag(Rest,[TagId]++Result)
	end.			 	

get_location() ->
	case catch mnesia:dirty_all_keys(tag_reader_info) of
		{'EXIT' ,Reason} ->
			io:format("EXIT_Error1: ~p",[Reason]),
			{error,Reason};
		[] ->
			hd(?LOCS);
		TagReaderIdsList ->
			location(TagReaderIdsList)
	end.
			
location(L) ->
	location(L,[]).
location([],RoomNums)->
	io:format("all : ~p~n loc: ~p~n",[?LOCS,RoomNums]),
	hd(?LOCS -- RoomNums);	
location([TagReaderId|Rest],RoomNum) ->
	[TagReaderRec] = mnesia:dirty_read(tag_reader_info,TagReaderId),
	Loc = TagReaderRec#tag_reader_info.location_id,			
	location(Rest,[Loc]++RoomNum). 


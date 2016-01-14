%% this service stores the data into the mysql database using the odbc driver

-module(spa_mysql_service).

-behaviour(gen_server).
-export([start_link/0,init/1,stop/0]).
-export([handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([add_tag/2,
		add_location/2,
		add_tag_reader/3,
		add_user/8,
		track_user_info/2,
		delete_user/1,
		delete_all_tables/0,
		ran_num/1]).

-define(DATA_SOURCE_NAME,"mysqldb").
-define(TIME_OUT_VALUE,60*60*1000).
-include("spa.hrl").


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE,{stop}).

%%"DSN=spa_db"
init([]) ->
	io:format("~nmysql starting ... ~n"),
	case catch odbc:start() of
		ok ->
			Connstring="DSN="++?DATA_SOURCE_NAME,
			case odbc:connect(Connstring,[]) of
				{ok,ConnRef} -> 
					{ok,ConnRef,?TIME_OUT_VALUE};
				{error,Reason} -> 
					odbc:stop(),
					{stop,Reason}
			end; 
		{error,{already_started,odbc}} ->
			Connstring="DSN="++?DATA_SOURCE_NAME,
			case odbc:connect(Connstring,[]) of
				{ok,ConnRef} ->
					{ok,ConnRef,?TIME_OUT_VALUE};
				{error,Reason} -> 
					odbc:stop(),
					{stop,Reason}
			end; 							
		{error,Reason} -> 
			{stop,Reason}
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
add_location(LocId,LocName) ->
	gen_server:call(?MODULE,{add_loc,LocId,LocName}).

add_tag(TagNum,TagId) ->
	gen_server:call(?MODULE,{add_tag,TagNum,TagId}).

add_tag_reader(ReaderNum,TagReaderId,LocationId) ->
	gen_server:call(?MODULE,{add_tag_reader,ReaderNum,TagReaderId,LocationId}).

add_user(TagId,Name,Age,Gender,Phone,Address,Occuptn,Status) ->
	gen_server:call(?MODULE,{add_user,TagId,Name,Age,Gender,Phone,Address,Occuptn,Status}).

track_user_info(TagId,TagReaderId) ->
	gen_server:call(?MODULE,{track_info,TagId,TagReaderId}).

delete_user(UserName) ->	
	gen_server:call(?MODULE,{delete_user,UserName}).	

delete_all_tables() ->	
	gen_server:call(?MODULE,{delete_tables}).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({add_loc,LocId,LocName},_From,State) ->
	add_location(LocId,LocName,State),
	{reply,{ok,"locations_updated_from_sql"},State};

handle_call({delete_tables},_From,State) ->
	delete_tables(?SQL_TABLES,State),
	{reply,{ok,"tables_deleted_from_sql"},State};

handle_call({add_tag,TagNum,TagId},_From,State)  ->
	Reply = insert_tag_info(TagNum,TagId,State),
	{reply,Reply,State};

handle_call({add_tag_reader,ReaderNum,TagReaderId,LocationId},_From,State)  ->
	Reply = insert_tag_reader_info(ReaderNum,TagReaderId,LocationId,State),
	{reply,Reply,State};

handle_call({add_user,TagId,Name,Age,Gender,Phone,Address,Occuptn,Status},
						_From,State)  ->
	Reply = insert_user_info(TagId,Name,Age,Gender,Phone,Address,Occuptn,Status,State),
	{reply,Reply,State};

handle_call({delete_user,UserName},_From,State) ->
	inactive_user(UserName,State),
	{reply,{ok,user_inactive_in_mysql},State};	

handle_call({track_info,TagId,TagReaderId},_From,State)  ->
	Reply = insert_track_info(TagId,TagReaderId,tim(),State),
	{reply,Reply,State};

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


insert_tag_reader_info(ReaderNum,TagReaderId,LocationId,ConnRef) ->
	ValStr = join_values([ReaderNum,to_int(TagReaderId),to_int(LocationId)],[]),
	SqlQuery = "insert into readerinfo values ("++ValStr++");",
	run_query(ConnRef,SqlQuery).

insert_tag_info(TagNum,TagId,ConnRef) ->
	ValStr = join_values([TagNum,TagId],[]),
	SqlQuery = "insert into taginfo values ("++ValStr++");",
	run_query(ConnRef,SqlQuery).	

insert_user_info(TagId,Name,Age,Gender,Phone,Address,Occuptn,Status,ConnRef) ->
	UserId = ran_num("user"),
	ValStr = join_values([UserId,Name,to_int(Age),Gender,Phone,Address,Occuptn,TagId,Status],[]),
	SqlQuery="insert into userinfo values ("++ValStr++");",
	run_query(ConnRef,SqlQuery).

insert_track_info(TagId,TagReaderId,TimeStamp,ConnRef) ->
	TrackId = ran_num("track"), 
	[Date,Time] =string:tokens(TimeStamp," "),
	ValStr = join_values([to_int(TrackId),to_int(TagReaderId),to_int(TagId),Date,Time],[]),
	SqlQuery="insert into trackinfo values ("++ValStr++");",
	run_query(ConnRef,SqlQuery).

delete_tables([],_ConnRef) ->
	io:format("~nAll the Tables info from Mysql have been deleted at time ~p~n",[tim()]),
	ok;
delete_tables([Tab|Rest],ConnRef) ->
	SqlQuery = "drop table "++ Tab++";",
	run_query(ConnRef,SqlQuery),
	delete_tables(Rest,ConnRef).

add_location(LocId,LocName,ConnRef) ->
	ValStr = join_values([to_int(LocId),LocName],[]),
	SqlQuery="insert into location values ("++ValStr++");",
	run_query(ConnRef,SqlQuery).

inactive_user(UserName,ConnRef) ->
	User = join_values([UserName],[]),
%%	MTS = join_values([tim()],[]),
	SqlQuery = "update userinfo set status = 'InActive' where Name ="++User++";",
	run_query(ConnRef,SqlQuery).	

run_query(ConnRef,SqlQuery) ->
	case odbc:sql_query(ConnRef,SqlQuery) of
		{updated,_} ->	
			io:format("~nSQL Query : ~p||Query Successful~n,",[SqlQuery]),
			ok;	
		{error,Reason} ->
			io:format("~nSQL Query : ~p||Query Failed||Reason:  ~p~n,",
				[SqlQuery,io_lib:print(Reason)]),
			{error,io_lib:print(Reason)}
	end.



tim() -> 
	{{Y,M,D},{H,Mi,S}} =erlang:localtime(),
	{YYYY,MM,DD} =to_bin({Y,M,D}),
	{HH,Min,Sec} = to_bin({H,Mi,S}),
	to_list(<<YYYY/binary,"-",MM/binary,"-",DD/binary," ",
				HH/binary,":",Min/binary,":",Sec/binary>>).

join_values([],Result) ->
	lists:reverse(Result),
	string:join(lists:reverse(Result),",");

join_values([H|T],Result) ->
%%	io:format("H ::~p~n",[H]),
	Value =	
	case H of 
		[] ->"NULL";
		H ->"'"++to_list(H)++"'"
	end,
	join_values(T,[Value]++Result).


to_list(A) ->
	spa_utils:to_list(A).

to_bin(A) ->
	spa_utils:to_bin(A).


ran_num("tag") ->
	random(121);
ran_num("reader") ->
	random(131);
ran_num("user") ->
	random(141);
ran_num("track") ->
	random(151).

random(Num) ->
	{_,_,Num1} = erlang:timestamp(),
	list_to_integer(to_list(Num)++to_list(Num1)).

to_int(A) when is_integer(A) ->
	A;
to_int(A) when is_list(A)->
	list_to_integer(A).



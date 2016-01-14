%% Mnesia service for storing the data into the mnesia tables
-module(spa_mnesia_service).
%%-export([create_tables/0]).

-behaviour(gen_server).
-export([start_link/0,init/1,stop/0]).
-export([handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([add_tag/2,add_tag/3,
		add_tag_reader/3,
		add_user/8,
		track_user_info/2,
		track_user_info/3,
		update_tag/1,
		update_tag/2,
		delete_user/1,
		delete_all_tables/0]).
-define(WAIT_TIME,5000).
-include("spa.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE,{stop}).

init([]) ->
	io:format("~nmnesia starting ... ~n"),
	mnesia:stop(),
	start_mnesia_db().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

add_tag(TagNum,TagId) ->
	add_tag(TagNum,TagId,[]). 		

add_tag(TagNum,TagId,Attributes) ->
	gen_server:call(?MODULE,{add_tag,TagNum,TagId,"No",Attributes}).

add_tag_reader(ReaderNum,TagReaderId,LocationId) ->
	gen_server:call(?MODULE,{add_tag_reader,ReaderNum,TagReaderId,LocationId}).


add_user(TagId,Name,Age,Gender,Phone,Address,Occuptn,Status) ->
	gen_server:call(?MODULE,{add_user,TagId,Name,Age,Gender,Phone,Address,Occuptn,Status}).


track_user_info(TagId,TagReaderId) ->
	track_user_info(TagId,TagReaderId,[]).

track_user_info(TagId,TagReaderId,Attributes) ->
	gen_server:call(?MODULE,{track_user,TagId,TagReaderId,Attributes}).

update_tag(TagId) ->
	update_tag(TagId,"Yes").

update_tag(TagId,Assigned) ->
	gen_server:call(?MODULE,{update_tag,TagId,Assigned}).

delete_user(UserName) ->	
	gen_server:call(?MODULE,{delete_user,UserName}).	

delete_all_tables() ->	
	gen_server:call(?MODULE,{delete_tables}).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({delete_user,UserName},_From,State) ->
	{ok,TagId} = delete_user_info(UserName),
	{reply,{ok,TagId},State};

handle_call({delete_tables},_From,State) ->
	delete_tables(?MNESIA_TABLES),
	{reply,{ok,"tables_deleted_from_Mnesia"},State};	

handle_call({add_tag,TagNum,TagId,Assigned,Attributes},_From,State)  ->
	mnesia:dirty_write(tags_info,#tags_info{tag_id=TagId,tag_num=TagNum,assigned = Assigned,
						attributes=Attributes,cdt = tim(),mdt = tim()}),
	{reply,{ok,"tag_added_to_Mnesia"},State};

handle_call({add_tag_reader,ReaderNum,TagReaderId,LocationId},_From,State)  ->
	mnesia:dirty_write(tag_reader_info,#tag_reader_info{tag_reader_id=TagReaderId,
						tag_reader_num=ReaderNum,location_id = LocationId,cdt = tim(),mdt = tim()}),
	{reply,{ok,"tag_reader_added_to_Mnesia"},State};

handle_call({add_user,TagId,Name,Age,Gender,Phone,Address,Occuptn,Status},
						_From,State)  ->
	mnesia:dirty_write(users_info,#users_info{tag_id = TagId,name=Name,
						age = Age,gender = Gender,phone=Phone,address = Address,
						occupation = Occuptn, status=Status,cdt = tim(),mdt = tim()}),
	{reply,{ok,"user_added_to_Mnesia"},State};


handle_call({track_user,TagId,TagReaderId,Attributes},_From,State)  ->
%%	io:format("")
	mnesia:dirty_write(track_user_info,#track_user_info{tag_id = TagId,tag_reader_id = TagReaderId,
						attributes=Attributes,timestamp= tim()}),
	{reply,{ok,"added_track_user_info_to_Mnesia"},State};

handle_call({update_tag,TagId,Assigned},_From,State)  ->
	update_tag_info(TagId,Assigned),
	{reply,"updated_tag_to_mnesia",State};

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
start_mnesia_db() ->	
	case start_mnesia() of
		ok ->
			create_all_tables(),
			{ok,[]};
		{error,Reason} ->
			{error,Reason}
	end.



start_mnesia() ->
	case catch mnesia:create_schema([node()|nodes()]) of
		ok ->
			mnesia:start();
		{error,{_,{already_exists,_}}} ->
			mnesia:start(),
			timer:sleep(?WAIT_TIME);
		{error,Reason} ->
			io:format("Reason111:~p~n",[Reason]),
			{error,Reason}
	end.

create_all_tables() ->
	Response = 
	case catch mnesia:create_table(tags_info,[{index, [tag_num]},{disc_copies,[node()|nodes()]},{attributes,record_info(fields,tags_info)}])of
		{atomic,ok} ->
			ok;
		{aborted,{already_exists,_}} ->
			io:format("already created.."),
			okk;
		{aborted,Reason} ->
			io:format("REason creating tables~p~n",[Reason]),
			{error,Reason}
	end,
	io:format("REsponse :: Tags_info~p~n",[Response]),
	mnesia:create_table(tag_reader_info,[{index, [tag_reader_num]},{disc_copies,[node()|nodes()]},{attributes,record_info(fields,tag_reader_info)}]),
	mnesia:create_table(users_info,[{index, [name]},{disc_copies,[node()|nodes()]},{attributes,record_info(fields,users_info)}]),
	mnesia:create_table(track_user_info,[{disc_copies,[node()|nodes()]},{attributes,record_info(fields,track_user_info)}]),
	io:format("Created all the Mnesia tables successfully ...."),
	ok.


tim() ->
	erlang:localtime().

update_tag_info(TagId,Assigned) ->
	case catch  mnesia:dirty_read(tags_info,TagId) of
		{'EXIT' ,Reason} ->
			io:format("EXIT_Error1: ~p",[Reason]),
			ok;
		[] ->
			io:format("~nError:No tag id record Available~n"),
			ok;
		[TagInfoRec] ->
			mnesia:dirty_write(TagInfoRec#tags_info{assigned=Assigned,mdt=tim()})
	end.	


delete_tables([]) ->
	io:format("~nAll the Tables info from Mnesia have been deleted at  time ~p~n",[tim()]),
	ok;
delete_tables([Tab|Rest]) ->
	mnesia:delete_table(Tab),
	delete_tables(Rest).


delete_user_info(UserName) ->
	case catch mnesia:dirty_index_read(users_info, UserName, #users_info.name) of
		{'EXIT',Reason} ->
			{error,Reason};
		[] -> 
			{ok,[]};
		[UserRec] -> 
			TagNum = UserRec#users_info.tag_id,
%%			io:format("Tagid after deleting:~p",[TagNum]),
			mnesia:dirty_delete(users_info,TagNum),
			[TagsInfoRec] = mnesia:dirty_index_read(tags_info, TagNum, #tags_info.tag_num),
			TagId = TagsInfoRec#tags_info.tag_id,
			{ok,TagId}
	end.




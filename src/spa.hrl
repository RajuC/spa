-record(location,
			{location_id,
			location_name}).

-record(tag_reader_info,
			{tag_reader_id,
			tag_reader_num,
			location_id,    %% room num assigned to a particular card reader
			cdt,
			mdt}).

-record(tags_info,{tag_id,
			tag_num,
			assigned, %% assigned to a user or not, initially no
			attributes, %% extra info
			cdt,
			mdt}).

-record(users_info,
			{tag_id,
			name,
			age,
			gender,
			phone,
			address,
			occupation,  %% admin or normal user
			status,%% extra information active or inactive 
			cdt,
			mdt}).

-record(track_user_info,
			{tag_id,
			tag_reader_id,									
			attributes,  %% extra information			
			timestamp}). 



-define(MNESIA_TABLES,[tag_reader_info,tags_info,track_user_info,
                      users_info]).
-define(SQL_TABLES,["TagInfo","ReaderInfo","UserInfo",
					"TrackInfo"]).
-define(LOCATION,[{"11111","DIARY"},{"22222","ELECTRONICS"},{"33333","COSMETICS"},{"44444","MOBILES"}]).

-define(LOCS,["11111","22222","33333","44444"]).
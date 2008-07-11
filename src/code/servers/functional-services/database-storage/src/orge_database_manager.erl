% Orge database manager.
% Stores most of the main persistant informations regarding connections,
% users, and simulated elements.
-module(orge_database_manager).

-export([start/1,start_link/1,start/2,start_link/2,
	user_settings_to_string/1,user_to_string/1,login_status_to_string/1]).



% Implementation notes.

% This database manager uses Mnesia to store the various informations 
% specified in OrgeDatabase.rst.
%
% To each table corresponds a dedicated record definition, and the two are
% named identically (useful with mnesia:write/1, for example).
% The first attribute of the record is always the primary key, most of the 
% time chosen to be an arbitrary identifer (more precisely: a counter).
%
% Access to the database will be transaction-based.
%
% The user table (orge_user) will be created in RAM and on disk as well.
% The connection table (orge_connection) will be created in RAM and on disk as
% well.


% Necessary to call QLC query language functions:
-include_lib("stdlib/include/qlc.hrl").

% For orge_user_settings and al:
-include("orge_database_manager.hrl").

% For emit_*:
-include("traces.hrl").


% Internal record that stores the current state of the Orge database:
-record( database_state, {
	current_user_id
} ).


% Shortcut macros, for convenience: 

-define(getState,DatabaseState#database_state).

-define(user_settings,OrgeUserSettings#orge_user_settings).
-define(user,OrgeUser#orge_user).



% Describes the Mnesia internal entry type (fields) for Orge users:
% (correspond to a non-nested orge_user_settings record, plus other fields).
% A Orge user can only have one Orge account.
% The identifier is a counter set by the database manager, starting from 1.
% account_status: in [active,suspended,deleted].
-record( orge_user, 
	{
	
		% Internal fields:
		identifier,
		account_creation_time,
		account_status,

% Character record: flags telling whether the controlling player authorized
% this character to be reinjected in-game, as NPC or monster.
% Each character should have an  textual physical and psychological description,
% of a few lines.
		characters,
		
		% orge_user_settings fields:
		first_name,
		last_name,
		date_of_birth,
		address_line_1,
		address_line_2,
		city,
		state,
		country,
		postal_code,
		home_telephone,
		mobile_telephone,
		email_address,
		account_login,
		account_password,
		security_question,
		security_answer
		
	}
).


% Describes the Mnesia internal entry type (fields) for Orge connections.
% The connection identifier is a counter set by the Orge server, starting from
% 1.
% login_status is in: [not_tried_yet, access_granted, bad_login, bad_password,
% timeout, marshalling_failed, already_connected, account_not_active].
% user_identifier is either an orge_user identifier, or undefined.
% login is the specified user login (if any)
% password is the specified user password (if any)
% ip_address is a {N1,N2,N3,N4} IPv4 address.
% port is an unsigned integer.
% Start and stop times are either a timestamp, or undefined.
% Note: depending on the login_status, some fields will be left to undefined.
-record( orge_connection, 
	{
		identifier,
		login_status,
		user_identifier,
		login,
		password,
		ip_address,
		port,
		start_time,
		stop_time,
		geolocated_country,
		geolocated_region,
		geolocated_city,
		geolocated_postal_code
	}
).



% List of all the tables of interest in the Orge database:
-define(table_list,[orge_user,orge_connection]).



% Starts an Orge database and returns its PID.
% InitMode can be:
%   - from_scratch: to start a fresh new (blank) database
%   - from_previous_state: to start a previously existing database, read from
% file
start(InitMode) ->
	spawn( fun() -> init(InitMode,no_listener) end ).


% Starts a linked Orge database and returns its PID.
% InitMode can be:
%   - from_scratch: to start a fresh new (blank) database
%   - from_previous_state: to start a previously existing database, read from
% file
start_link(InitMode) ->
	spawn_link( fun() -> init(InitMode,no_listener) end ).



% Starts an Orge database and returns its PID. 
% Will notify specified listener whenever the database will be up and
% running.
% InitMode can be:
%   - from_scratch: to start a fresh new (blank) database
%   - from_previous_state: to start a previously existing database, read from
% file
start(InitMode,ListenerPid) ->
	spawn( fun() -> init(InitMode,ListenerPid) end ).


% Starts a linked Orge database and returns its PID. 
% Will notify specified listener whenever the database will be up and
% running.
% InitMode can be:
%   - from_scratch: to start a fresh new (blank) database
%   - from_previous_state: to start a previously existing database, read from
% file
start_link(InitMode,ListenerPid) ->
	spawn_link( fun() -> init(InitMode,ListenerPid) end ).



		
% Helper functions.


% init returns the first database state.
init(from_scratch,ListenerPid) ->
	?emit_info([ "Starting Orge database, created from scratch." ]),
	start_geolocation_service(),
	% Includes only this node:
	DatabaseNodes = get_database_nodes(),
	% Ignore failure if no schema was already existing:
	case mnesia:delete_schema( DatabaseNodes ) of 
	
		ok ->
			?emit_info([ "A previous database has been deleted." ]);

		{error,{Reason,Arg}} ->
			?emit_info([ io_lib:format( 
				"No previous database deleted: ~s: ~w.",
				[Reason,lists:flatten(Arg)] ) ]);
		
		{error,Reason} ->
			?emit_info([ io_lib:format( "No previous database deleted: ~w.",
				[Reason] ) ])
	
	end,
	?emit_trace( [ "Creating database schema." ] ),
    ok = mnesia:create_schema( DatabaseNodes ),
	?emit_trace( [ "Starting database." ] ),
    ok = mnesia:start(),
	create_tables_on( DatabaseNodes ),
	notify_ready(ListenerPid),
	?emit_trace( [ "Orge database ready." ] ),
	loop( #database_state{ current_user_id=1 } );
	
init(from_previous_state,ListenerPid) ->	
	?emit_info([ "Starting Orge database from previous state." ]),
	ok = mnesia:start(),
	start_geolocation_service(),
	TargetTables = ?table_list,
 	?emit_debug([ io_lib:format( 
		"Waiting for the loading of following tables: ~w.",
		[TargetTables] ) ]),
	% Hangs until all tables in the list are accessible, or until the
	% time-out occurs (milliseconds):
	ok = mnesia:wait_for_tables(TargetTables, 4000),
	notify_ready(ListenerPid),
	?emit_trace( [ "Orge database ready." ] ),
	loop( #database_state{ current_user_id=1 } ).


start_geolocation_service() ->
	?emit_trace( [ "Starting IP geolocation service." ] ),
	{ok,_Pid} = egeoip:start().


notify_ready(no_listener) ->
	ok;
	
notify_ready(ListenerPid) ->
	ListenerPid ! orge_database_ready.
	
	
	
% The database main loop.
loop( DatabaseState ) ->
	?emit_debug([ "Database waiting for requests." ]),
	receive
	
		% User-related actions.

		{try_login,{Login,Password},ConnectionId,ClientNetId,CallerPid} ->
			Answer = try_login(Login,Password,ConnectionId,ClientNetId),
			CallerPid ! {ConnectionId,Answer},
			loop( DatabaseState );
	
		{declare_end_of_session,ConnectionId} ->	
			record_end_of_session(ConnectionId),
			loop( DatabaseState );

		{declare_marshalling_failed,ConnectionId,ClientNetId} ->
			record_marshalling_failure(ConnectionId,ClientNetId),
			loop( DatabaseState );
		
		{declare_timeout,ConnectionId,ClientNetId} ->
			record_timeout(ConnectionId,ClientNetId),
			loop( DatabaseState );
		
		{register_user,UserSettings,CallerPid} ->
			loop( register_user( DatabaseState,UserSettings,CallerPid ) );
			
		{unregister_user,UserLogin,CallerPid} ->
			loop( unregister_user( DatabaseState,UserLogin,CallerPid ) );
		
		
		% Listing of informations.
		
		{list_users,CallerPid} ->
			CallerPid ! { orge_users, list_users() },
			loop( DatabaseState );

		{list_connections,CallerPid} ->
			CallerPid ! { orge_connections, list_connections() },
			loop( DatabaseState );
		
		{list_active_connections,CallerPid} ->
			CallerPid ! { orge_active_connections, list_active_connections() },
			loop( DatabaseState );
		
		
		reset ->
			loop( reset_tables(DatabaseState) );
				
		{stop,CallerPid} ->
			stop(DatabaseState,CallerPid);
		
		Other ->
			?emit_warning([ io_lib:format( 
				"Database ignored following message: ~w.", [Other] ) ])
	
	end.


% Registers specified Orge user settings, where NewUserSettings is an 
% orge_user_settings record, in one transaction.
% The caller is notified of the result of the operation: user_registered or
% {user_registration_failed,AtomReason} is sent back, where AtomReason may be
% login_already_registered, write_transaction_failed, etc.
% Returns an updated state.
register_user(DatabaseState,NewUserSettings,CallerPid) ->
	case check_validity(NewUserSettings,DatabaseState) of 
	
		ok -> 
			ThisUserId = ?getState.current_user_id, 
			NewUser = update_from_settings( 
				#orge_user{  
					identifier = ThisUserId,
					account_creation_time = utils:get_timestamp(),
					account_status = active,
					characters = none
				}, NewUserSettings ),	
			F = fun() -> mnesia:write( NewUser ) end,
			case mnesia:transaction(F) of
			
				{atomic,_} ->
					CallerPid ! user_registered,
					?getState{ 
						current_user_id = ThisUserId + 1 };
				
				{aborted,FailReason} ->
					?emit_error([ io_lib:format(
						"Registering of user settings (~s) failed: ~w",
						[user_settings_to_string(NewUserSettings),
							FailReason] ) ]),
					CallerPid !
						{user_registration_failed,write_transaction_failed},
					DatabaseState
					
			end;		

		RejectReason ->
			?emit_warning([ io_lib:format(
				"Registering of user settings (~s) failed: ~w",
				[user_settings_to_string(NewUserSettings),
					RejectReason] ) ]),
			CallerPid ! {user_registration_failed,RejectReason},
			DatabaseState
		
	end.	
	

% Unregisters an Orge user, specified by login. Its account will be marked as
% deleted.
% The caller is notified of the result of the operation: user_unregistered  or
% {user_unregistration_failed,AtomReason} is sent back, where AtomReason may be
% login_not_known, account_already_deleted, write_transaction_failed, etc.
% Returns an updated state.
unregister_user( DatabaseState,UserLogin,CallerPid ) ->
	case get_user_from_login(UserLogin) of
	
		not_found ->
			?emit_warning([ io_lib:format(
				"Unregistering of user failed: unknown login '~s'",
				[UserLogin] ) ]),
			CallerPid ! {user_unregistration_failed,login_not_known},
			DatabaseState;
		
		UserInfos ->
			case UserInfos#orge_user.account_status of
			
				deleted ->
					?emit_warning([ io_lib:format(
						"Unregistering of user failed for login '~s': "
						"account already deleted", [UserLogin] ) ]),
					CallerPid !
						{user_unregistration_failed,account_already_deleted},
					DatabaseState;
					
				_Other ->
					NewUserInfos = UserInfos#orge_user{
						account_status = deleted
					},
					F = fun() -> mnesia:write( NewUserInfos ) end,
					case mnesia:transaction(F) of
			
						{atomic,_} ->
							CallerPid ! user_unregistered,
							DatabaseState;
				
						{aborted,FailReason} ->
							?emit_error([ io_lib:format(
								"Unregistering of user failed for login '~s': "
								"~w", [UserLogin,	FailReason] ) ]),
							CallerPid ! {user_unregistration_failed,
								write_transaction_failed},
							DatabaseState
					
					end
					
			
			end
			
	end.

	
	
list_users() ->
	list_table(orge_user).
	
	
list_connections() ->
	list_table(orge_connection).
	
	
list_active_connections() ->
	fixme.
	
	
% Lists all entries of specified table.
% Returns the entry list.
list_table(TableName) ->
	manage_query( qlc:q([ X || X <- mnesia:table(TableName) ]) ).


% Deletes all entries in the Orge tables.
reset_tables(DatabaseState) ->
	?emit_info([ "Resetting tables of Orge database." ]),
	lists:foreach( 
		fun(Table) -> 
			{atomic, ok} = mnesia:clear_table(Table)
		end,
		?table_list ),
	DatabaseState.


% Stops the database.
stop(_DatabaseState,CallerPid) ->
	?emit_info([ "Stopping Orge database." ]),
    mnesia:stop(),
	CallerPid ! orge_database_stopped.


% Includes only this node for the moment:
get_database_nodes() ->
	[node()].
	


% Database prebuilt convenience queries.

% Returns either not_found, or the orge_user record corresponding to specified
% login. 	
get_user_from_login( AccountLogin ) ->
	case manage_query( qlc:q([ X || X <- mnesia:table(orge_user),
			X#orge_user.account_login =:= AccountLogin ]) ) of 
		
		[] ->
			not_found;
			
		% At most one entry should match:	
		[UserInfo] ->
			UserInfo	
			
	end	.


% Returns either no_connection, or the orge_connection record corresponding
% to specified Orge connection identifier. 	
get_connection( ConnectionId ) ->
	case manage_query( qlc:q([ X || X <- mnesia:table(orge_connection),
			X#orge_connection.identifier =:= ConnectionId ]) ) of 
		
		[] ->
			no_connection;
			
		% At most one entry should match:	
		[Connection] ->
			Connection	
			
	end	.


% Returns either not_connected, or the orge_connection record corresponding
% to specified Orge user identifier, supposed to be already connected. 	
get_running_connection_for( UserId ) ->
	case manage_query( qlc:q([ X || X <- mnesia:table(orge_connection),
			X#orge_connection.user_identifier =:= UserId,
			X#orge_connection.login_status =:= active ]) ) of 
		
		[] ->
			not_connected;
			
		% At most one entry should match:	
		[Connection] ->
			Connection	
			
	end	.
	
	

% record_info cannot take variable arguments.
%create_tables_on(TableList,DatabaseNodes) ->
%	lists:foreach( fun(TableName) ->
%		{atomic, ok} = mnesia:create_table( TableName, [ 
%				{attributes, record_info(fields,TableName)},
%				{disc_copies,DatabaseNodes},
%				{type,set}
%			] )	end,
%	TableList).		

create_tables_on(DatabaseNodes) ->

	?emit_trace([ io_lib:format( "Creating tables on following node(s): ~w.",
		[DatabaseNodes] ) ] ),
	
	?emit_debug([ "Creating table for Orge users." ]),
	{atomic,ok} = mnesia:create_table( orge_user, [ 
		{attributes, record_info(fields,orge_user)},
		{disc_copies,DatabaseNodes},
		{type,set} ] ),
		
	?emit_debug([ "Creating table for Orge connections." ]),
	{atomic,ok} = mnesia:create_table( orge_connection, [ 
		{attributes, record_info(fields,orge_connection)},
		{disc_copies,DatabaseNodes},
		{type,set} ] ).		
	

% Returns the orge_user record OrgeUser, whose fields have been updated 
% from the OrgeUserSettings orge_user_settings record.
update_from_settings( OrgeUser, OrgeUserSettings ) ->
	?user{
		first_name        = ?user_settings.first_name,
		last_name         = ?user_settings.last_name,
		date_of_birth     = ?user_settings.date_of_birth,
		address_line_1    = ?user_settings.address_line_1,
		address_line_2    = ?user_settings.address_line_2,
		city              = ?user_settings.city,
		state             = ?user_settings.state,
		country           = ?user_settings.country,
		postal_code       = ?user_settings.postal_code,
		home_telephone    = ?user_settings.home_telephone,
		mobile_telephone  =
			?user_settings.mobile_telephone,
		email_address     = ?user_settings.email_address,
		account_login     = ?user_settings.account_login,
		account_password  = 
			?user_settings.account_password,
		security_question =
			?user_settings.security_question,
		security_answer   = ?user_settings.security_answer
	}.
	
	
% Manages a QLC query.	
manage_query(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
 	

% Returns a textual description of specified Orge user settings record.
user_settings_to_string(OrgeUserSettings) ->
	io_lib:format( 
		"User first name is '~s', last name is '~s', date of birth is '~s', "
		"first address line is '~s', second one is '~s', city is '~s', "
		"state is '~s', country is '~s', postal code is '~s', "
		"home telephone is '~s', mobile one is '~s', e-mail address is '~s', "
		" account login is '~s', account password is '~s', "
		"security question is '~s', security answer is '~s'",
		[ 
			?user_settings.first_name,
			?user_settings.last_name,
			?user_settings.date_of_birth,
			?user_settings.address_line_1,
			?user_settings.address_line_2,
			?user_settings.city,
			?user_settings.state,
			?user_settings.country,
			?user_settings.postal_code,
			?user_settings.home_telephone,
			?user_settings.mobile_telephone,
			?user_settings.email_address,
			?user_settings.account_login,
			?user_settings.account_password,
			?user_settings.security_question,
			?user_settings.security_answer
			
		] ).


% Returns a textual description of specified Orge user record.
user_to_string(OrgeUser) ->
	io_lib:format( "Account #~s created on ~s with registered characters ~w. "
		"Current account status: ~s. "
		"User first name is '~s', last name is '~s', date of birth is '~s', "
		"first address line is '~s', second one is '~s', city is '~s', "
		"state is '~s', country is '~s', postal code is '~s', "
		"home telephone is '~s', mobile one is '~s', e-mail address is '~s', "
		" account login is '~s', account password is '~s', "
		"security question is '~s', security answer is '~s'",
		[ 
			utils:integer_to_string( ?user.identifier ),
			utils:timestamp_to_string( 
				?user.account_creation_time ),
			?user.characters,
			?user.account_status,
			?user.first_name,
			?user.last_name,
			?user.date_of_birth,
			?user.address_line_1,
			?user.address_line_2,
			?user.city,
			?user.state,
			?user.country,
			?user.postal_code,
			?user.home_telephone,
			?user.mobile_telephone,
			?user.email_address,
			?user.account_login,
			?user.account_password,
			?user.security_question,
			?user.security_answer
			
		] ).




% Returns a textual description of specified Orge connection record.
% Note: no direct patter-matching on login_status to avoid to have to bind 
% specifically each field of interest.
connection_to_string(OrgeConnection) ->
	String = case OrgeConnection#orge_connection.login_status of
	
		not_tried_yet ->
			untried_connection_to_string(OrgeConnection);
			 
		access_granted ->
			connection_granted_to_string(OrgeConnection);
			
		bad_login ->
			connection_bad_login_to_string(OrgeConnection);
			
		bad_password ->
			connection_bad_password_to_string(OrgeConnection);

		timeout ->
			connection_timeout_to_string(OrgeConnection);
			
		marshalling_failed ->
			connection_marshalling_failed_to_string(OrgeConnection);
			
		already_connected ->
			connection_already_existing_to_string(OrgeConnection);
		
		account_not_active ->
			connection_no_active_account_to_string(OrgeConnection)
			
	end,
	String ++ " " ++ geolocation_to_string(OrgeConnection).
	
	
untried_connection_to_string(OrgeConnection) ->
	io_lib:format( "Unregistered connection #~s at ~s from host ~s.",
		[ 
			utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time ),
			utils:ipv4_to_string( OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port	)
		] ).


connection_granted_to_string(OrgeConnection) ->
	StopSentence = case OrgeConnection#orge_connection.stop_time of 
	
		undefined ->
			"still active";
		
		Timestamp ->
			io_lib:format( "stopped at ~s", 
				[ utils:timestamp_to_string(Timestamp) ] )
	
	end,
	io_lib:format( "Successful connection #~s of Orge user #~s "
		"(login: '~s', password: '~s'), from host ~s, started at ~s, ~s.",
		[ 
			utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			utils:integer_to_string(
				OrgeConnection#orge_connection.user_identifier ),
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.password,
			utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time ),
			StopSentence
		] ).
			
			
connection_bad_login_to_string(OrgeConnection) ->
	io_lib:format( "Failed connection #~s due to bad login '~s' "
		"(with specified password '~s') from host ~s at ~s.",
		[ 
			utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.password,		
			utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time )
		] ).


connection_bad_password_to_string(OrgeConnection) ->
	io_lib:format( "Failed connection #~s due to bad password '~s' "
		"for known login '~s' from host ~s at ~s.",
		[ 
			utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			OrgeConnection#orge_connection.password,		
			OrgeConnection#orge_connection.login,
			utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time )
		] ).


connection_timeout_to_string(OrgeConnection) ->
	io_lib:format( "Time-out for connection #~s at ~s from host ~s.",
		[ 
			utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time ),
			utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port	)
		] ).


connection_marshalling_failed_to_string(OrgeConnection) ->
	io_lib:format( "Failed marshalling for connection #~s at ~s from host ~s.",
		[ 
			utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time ),
			utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port	)
		] ).


connection_already_existing_to_string(OrgeConnection) ->
	io_lib:format( "Failed connection #~s due to an already existing connection"
		" for Orge user #~s (login: '~s', password: '~s'), from host ~s, "
		"at ~s.",
		[ 
			utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			utils:integer_to_string(
				OrgeConnection#orge_connection.user_identifier ),
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.password,		
			utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time )
		] ).


connection_no_active_account_to_string(OrgeConnection) ->
	io_lib:format( "Failed connection #~s due to a non-active account"
		" for Orge user #~s (login: '~s', password: '~s'), from host ~s, "
		"at ~s.",
		[ 
			utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			utils:integer_to_string(
				OrgeConnection#orge_connection.user_identifier ),
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.password,		
			utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time )
		] ).
		

login_status_to_string(not_tried_yet) ->
	"no login attempt performed yet";
	
login_status_to_string(access_granted) ->
	"successfully logged-in";
	
login_status_to_string(bad_login) ->
	"failed login due to bad login";
	
login_status_to_string(bad_password) ->
	"failed login due to bad password";
	
login_status_to_string(timeout) ->
	"failed login due to client time-out";
	
login_status_to_string(marshalling_failed) ->
	"failed login due to client ill-formatted identifier data";
	
login_status_to_string(already_connected) ->
	"failed login since user already logged-in";
		
login_status_to_string(account_not_active) ->
	"failed login since user account not active".
		

% Returns a textual description of IP geolocation information stored in
% specified connection.
geolocation_to_string(Connection) ->
	Country = case Connection#orge_connection.geolocated_country of
	
		[] ->
			"no country could be guessed";
		
		GuessedCountry ->	
			io_lib:format( "guessed country is ~s", [GuessedCountry] )
			
	end,
	
	Region = case Connection#orge_connection.geolocated_region of
	
		[] ->
			"no region could be guessed";
		
		GuessedRegion ->	
			io_lib:format( "guessed region is ~s", [GuessedRegion] )
			
	end,
	
	City = case Connection#orge_connection.geolocated_city of
	
		[] ->
			"no city could be guessed";
		
		GuessedCity ->	
			io_lib:format( "guessed city is ~s", [GuessedCity] )
			
	end,
	
	
	PostalCode = case Connection#orge_connection.geolocated_postal_code of
	
		[] ->
			"no postal code could be guessed";
		
		GuessedPostalCode ->	
			io_lib:format( "guessed postal code is ~s", [GuessedPostalCode] )
			
	end,
	io_lib:format( "IP geolocation informations: ~s, ~s, ~s, ~s", 
		[Country, Region, City, PostalCode] ).
	

% Section for user informations validity checking (on initial registration).


% First check, avoids too much nesting.
check_validity(NewUserSettings,DatabaseState) ->
	case check_validity_of_personal_infos(
			NewUserSettings#orge_user_settings.first_name,
			NewUserSettings#orge_user_settings.last_name,
			NewUserSettings#orge_user_settings.date_of_birth ) of 
		
		ok ->
			second_validity_check( NewUserSettings,DatabaseState );
				
		Other ->
			Other			
	
	end.
	
	
second_validity_check(NewUserSettings,DatabaseState) ->
	case check_validity_of_actual_address(
			NewUserSettings#orge_user_settings.address_line_1,
			NewUserSettings#orge_user_settings.address_line_2,
			NewUserSettings#orge_user_settings.city,
			NewUserSettings#orge_user_settings.state,
			NewUserSettings#orge_user_settings.country,
			NewUserSettings#orge_user_settings.postal_code ) of 
					
		ok ->
			third_validity_check( NewUserSettings,DatabaseState );	
	
		Other ->
			Other			
				
	end.


third_validity_check(NewUserSettings,DatabaseState) ->
	case check_validity_of_phones(
			NewUserSettings#orge_user_settings.home_telephone,
			NewUserSettings#orge_user_settings.mobile_telephone	) of 
	
		ok ->
			fourth_validity_check( NewUserSettings,DatabaseState );	
					
		Other ->
			Other			

	end.
	

fourth_validity_check(NewUserSettings,DatabaseState) ->
	case check_validity_of_email(
			NewUserSettings#orge_user_settings.email_address ) of 
	
		ok ->
			fifth_validity_check( NewUserSettings,DatabaseState );	
					
		Other ->
			Other			

	end.
	
	
fifth_validity_check(NewUserSettings,DatabaseState) ->
	case check_validity_of_account(
			NewUserSettings#orge_user_settings.account_login,
			NewUserSettings#orge_user_settings.account_password	) of 
	
		ok ->
			sixth_validity_check( NewUserSettings,DatabaseState );	
					
		Other ->
			Other			

	end.
	
	
sixth_validity_check(NewUserSettings,_DatabaseState) ->
	case check_validity_of_security_challenge(
			NewUserSettings#orge_user_settings.security_question,
			NewUserSettings#orge_user_settings.security_answer ) of 
	
		ok ->
			ok;	
					
		Other ->
			Other			

	end.
	
	
check_validity_of_personal_infos( _FirstName, _LastName, _DateOfBirth ) ->
	% TO-DO
	ok.		
	
	
check_validity_of_actual_address( _AddressLine1, _AddressLine2, _City,
		_State, _Country, _PostalCode )	->
	% TO-DO
	ok.
	
	
check_validity_of_phones( _HomeTelephone, _MobileTelephone ) ->
	% TO-DO
	ok.	
	
	
check_validity_of_email( _EmailAddress ) ->
	% TO-DO
	ok.
	
		
% Once a login has been registered (even if afterwards the associated account
% was marked as deleted), it cannot be registered again. 		
check_validity_of_account( AccountLogin, _AccountPassword ) ->
	% TO-DO: handle too short passwords.
	case get_user_from_login( AccountLogin ) of 
	
		not_found ->
			ok;
			
		_Other ->
			login_already_registered	
	
	end.
	
	
check_validity_of_security_challenge( _SecurityQuestion, _SecurityAnswer ) ->
	% TO-DO
	ok.
	


% Section for login validity checking.


% Checks specified login request, logs it appropriately in database and 
% performs the login if successful.
try_login( Login ,Password, ConnectionId, ClientNetId ) ->
	% First, check that login is known and that password matches:
	case get_user_from_login(Login) of
	
		not_found ->
			record_bad_login(Login,Password,ConnectionId,ClientNetId);
			
		% Only AccountStatus is not already bound:
		#orge_user{ identifier=UserId, account_password=Password,
				account_status = AccountStatus } ->
			
			case AccountStatus of 
			
				active ->
			
					% Login/password ok, account active, check this user is
					% not already connected currently:
					case get_running_connection_for(UserId) of
			
						not_connected ->
							record_access_granted( Login, Password,
								ConnectionId, ClientNetId, UserId );
													
						_Connection ->
							record_already_connected( Login, Password,
								ConnectionId, ClientNetId, UserId )
					
					end;
			
				_OtherAccountStatus ->
			
					record_account_non_active( Login, Password,
						ConnectionId, ClientNetId, UserId )
					
			end;
					
		_Other ->
			% We have here a match, but with a different password:
			record_bad_password( Login ,Password, ConnectionId, ClientNetId )
			
	end.
	



% Records that connection and returns an atom describing its status.
record_access_granted( Login, Password, ConnectionId, 
		{ClientIP,ClientPort}, UserId ) ->

	NewConnection = #orge_connection{
		identifier = ConnectionId,
		login_status = access_granted,
		user_identifier = UserId,
		login = Login,
		password = Password,
		ip_address = ClientIP,
		port = ClientPort,
		start_time = utils:get_timestamp(),
		stop_time = undefined
	},
	LocatedConnection = add_geolocation_infos(NewConnection),
	F = fun() -> mnesia:write( LocatedConnection ) end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			access_granted;
							
		{aborted,FailReason} ->
			?emit_error([ io_lib:format(
				"Registering of granted connection (~s) failed: ~w",
					[connection_to_string(NewConnection),FailReason] ) ]),
			internal_error	
	
	end.	


% Records that connection and returns an atom describing its status.
record_bad_login( Login, Password, ConnectionId, {ClientIP,ClientPort} ) ->
	NewConnection = #orge_connection{
		identifier = ConnectionId,
		login_status = bad_login,
		%user_identifier
		login = Login,
		password = Password,
		ip_address = ClientIP,
		port = ClientPort,
		start_time = utils:get_timestamp()
		%stop_time
	},
	LocatedConnection = add_geolocation_infos(NewConnection),
	F = fun() -> mnesia:write( LocatedConnection ) end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			bad_login;
							
		{aborted,FailReason} ->
			?emit_error([ io_lib:format(
				"Registering of connection with bad login (~s) failed: ~w",
					[connection_to_string(NewConnection),FailReason] ) ]),
			internal_error	
	
	end.	
	
	
	
% Records that connection and returns an atom describing its status.
record_bad_password( Login, Password, ConnectionId, {ClientIP,ClientPort} ) ->
	NewConnection = #orge_connection{
		identifier = ConnectionId,
		login_status = bad_password,
		%user_identifier
		login = Login,
		password = Password,
		ip_address = ClientIP,
		port = ClientPort,
		start_time = utils:get_timestamp()
		%stop_time
	},
	LocatedConnection = add_geolocation_infos(NewConnection),
	F = fun() -> mnesia:write( LocatedConnection ) end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			bad_password;
							
		{aborted,FailReason} ->
			?emit_error([ io_lib:format(
				"Registering of connection with bad password (~s) failed: ~w",
					[connection_to_string(NewConnection),FailReason] ) ]),
			internal_error	
	
	end.	

	
% Records that connection and returns an atom describing its status.
record_timeout( ConnectionId, {ClientIP,ClientPort} ) ->
	NewConnection = #orge_connection{
		identifier = ConnectionId,
		login_status = timeout,
		%user_identifier
		%login
		%password
		ip_address = ClientIP,
		port = ClientPort,
		start_time = utils:get_timestamp()
		%stop_time
	},
	LocatedConnection = add_geolocation_infos(NewConnection),
	F = fun() -> mnesia:write( LocatedConnection ) end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			timeout;
							
		{aborted,FailReason} ->
			?emit_error([ io_lib:format(
				"Registering of connection with time-out (~s) failed: ~w",
					[connection_to_string(NewConnection),FailReason] ) ]),
			internal_error	
	
	end.	


% Records that connection and returns an atom describing its status.
% Garbled identifiers could be stored maybe.
record_marshalling_failure(ConnectionId,{ClientIP,ClientPort}) ->
	NewConnection = #orge_connection{
		identifier = ConnectionId,
		login_status = marshalling_failed,
		%user_identifier
		%login
		%password
		ip_address = ClientIP,
		port = ClientPort,
		start_time = utils:get_timestamp()
		%stop_time
	},
	LocatedConnection = add_geolocation_infos(NewConnection),
	F = fun() -> mnesia:write( LocatedConnection ) end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			marshalling_failed;
							
		{aborted,FailReason} ->
			?emit_error([ io_lib:format(
				"Registering of connection with marshalling failure "
				"(~s) failed: ~w",
				[connection_to_string(NewConnection),FailReason] ) ]),
			internal_error	
	
	end.	

	
% Records that connection and returns an atom describing its status.
record_already_connected( Login, Password, ConnectionId, 
		{ClientIP,ClientPort}, UserId ) ->
	NewConnection = #orge_connection{
		identifier = ConnectionId,
		login_status = already_connected,
		user_identifier = UserId,
		login = Login,
		password = Password,
		ip_address = ClientIP,
		port = ClientPort,
		start_time = utils:get_timestamp()
		%stop_time
	},
	LocatedConnection = add_geolocation_infos(NewConnection),
	F = fun() -> mnesia:write( LocatedConnection ) end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			already_connected;
							
		{aborted,FailReason} ->
			?emit_error([ io_lib:format( 
				"Registering of already existing connection (~s) failed: ~w",
				[connection_to_string(NewConnection),FailReason] ) ]),
			internal_error	
	
	end.	


% Records that connection and returns an atom describing its status.
record_account_non_active( Login, Password,	ConnectionId, 
		{ClientIP,ClientPort}, UserId ) ->
	NewConnection = #orge_connection{
		identifier = ConnectionId,
		login_status = account_not_active,
		user_identifier = UserId,
		login = Login,
		password = Password,
		ip_address = ClientIP,
		port = ClientPort,
		start_time = utils:get_timestamp()
		%stop_time
	},
	LocatedConnection = add_geolocation_infos(NewConnection),
	F = fun() -> mnesia:write( LocatedConnection ) end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			account_not_active;
							
		{aborted,FailReason} ->
			?emit_error([ io_lib:format( 
				"Registering of connection failed due to a non-active account "
				"(~s) failed: ~w",
				[connection_to_string(NewConnection),FailReason] ) ]),
			internal_error	
	
	end.	
	
	
% Updates that connection and returns an atom describing its status.
record_end_of_session(ConnectionId) ->
	% Check that session exists and is recorded as active:
	Connection = get_connection(ConnectionId),
	% Update its entry (no login_status change):
	NewConnection = Connection#orge_connection { 
		stop_time = utils:get_timestamp()
	},
	LocatedConnection = add_geolocation_infos(NewConnection),
	F = fun() -> mnesia:write( LocatedConnection ) end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			end_of_session;
							
		{aborted,FailReason} ->
			?emit_error([ io_lib:format( 
				"Updating of ended connection (~s) failed: ~w",
				[connection_to_string(NewConnection),FailReason] ) ]),
			internal_error	
	
	end.	


% Updates connection records with IP geolocation informations.
add_geolocation_infos(Connection) ->
	TargetAddress = utils:ipv4_to_string( 
		Connection#orge_connection.ip_address ),
	case egeoip:lookup( TargetAddress ) of
	
		{ ok, {geoip,_CountryCode, _OtherCountryCode, CountryName, Region, City,
				PostalCode, _Latitude, _Longitude, _AreaCode, _DMACode } } ->
			Connection#orge_connection{
				geolocated_country = CountryName,
				geolocated_region = binary_to_list(Region),
				geolocated_city = binary_to_list(City),
				geolocated_postal_code = binary_to_list(PostalCode)
			};
			
		{ error, Reason } ->
			?emit_error([ io_lib:format( "Geolocation of ~s failed: ~w",
				[TargetAddress,Reason] ) ]),
			Connection
			
	end.				
		

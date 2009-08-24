% 
% Copyright (C) 2003-2009 Olivier Boudeville
%
% This file is part of the Orge library.
%
% The Orge library is free software: you can redistribute it and/or modify
% it under the terms of either the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option) 
% any later version.
%
% The Orge library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License and of the GNU General Public License along with the Orge library.
% If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Orge database manager.
% Stores most of the main persistent information regarding connections,
% users, and simulated elements.
-module(orge_database_manager).


-export([ start/1, start_link/1, start/2, start_link/2,
	user_settings_to_string/1, user_to_string/1, status_to_string/1 ]).




% Implementation notes.

% This database manager uses Mnesia to store the information specified in
% OrgeDatabase.rst.
%
% To each table (ex: user_settings) corresponds a dedicated record definition,
% and the two are named identically (useful with mnesia:write/1, for example).
%
% The first attribute of the record is always the primary key, most of the 
% time chosen to be an arbitrary identifier (more precisely: a counter).
%
% Accesses to the database will be transaction-based.
%
% The user table (orge_user) will be created in RAM and on disk as well.
% The connection table (orge_connection) will be created in RAM and on disk as
% well.

% mnesia:read, mnesia:select and qlc are preferred in this decreasing order,
% according to 
% http://erlang.org/pipermail/erlang-questions/2009-January/040922.html



% Necessary to call QLC query language functions:
-include_lib("stdlib/include/qlc.hrl").


% For orge_user_settings and al:
-include("orge_database_manager.hrl").


% For notify_*:
-include("traces.hrl").



% Internal record that stores the current state of the Orge database:
% (of course almost all the state is in the tables)
-record( database_state, {

	% The identifier that will be assigned to the next new user:
	current_user_id,
	
	% The maximum number of server slots (simultaneous active connections):
	maximum_slot_count
	
} ).



% Shortcut macros, for convenience: 

-define(getState,DatabaseState#database_state).

-define(user_settings,OrgeUserSettings#orge_user_settings).
-define(user,OrgeUser#orge_user).



% Describes the Mnesia internal entry type (fields) for Orge users:
% (corresponds to a non-nested orge_user_settings record, plus other fields).
% A Orge user can only have one Orge account.
% The identifier is a counter set by the database manager, starting from 1.
-record( orge_user, 
	{
	
		% Internal fields (i.e. they are not orge_user_settings fields):
		
		% The identifier of that user (unsigned integer); this is a counter
		% set by the database manager, starting from 1:
		identifier,
		
		% Timestamp corresponding to the creation time of this account:
		account_creation_time,

		% Timestamp corresponding to the deletion time of this account:
		account_deletion_time,
		
		% Current status of the account, in [active,suspended,deleted]:
		account_status,

		% List of the identifiers of the characters the user can control: 
		character_id_list,
		
		% orge_user_settings expanded fields:
		% (see orge_database_manager.hrl for their specification)
		first_name,
		last_name,
		date_of_birth,
		address_line_1,
		address_line_2,
		city,
		state,
		country,
		postal_code,
		home_phone,
		mobile_phone,
		email_address,
		account_login,
		account_password,
		security_question,
		security_answer
		
	}
).



% Describes an Orge character, i.e. a creature that can be controlled by
% a player. 
-record( orge_character, 
	{

		name,
		
		% Character record: flags telling whether the controlling player
		% authorized this character to be reinjected in-game, as NPC or monster.
		% Each character should have a textual physical and psychological
		% description, of a few lines.
		reuse,
		
	}
).



% Describes the Mnesia internal entry type (fields) for an Orge connection.
% Note: depending on the status, some fields will be left to undefined.
-record( orge_connection, 
	{
	
		% The connection identifier is a counter set by the Orge server,
		% starting from 1:
		identifier,
		
		% Current status of the connection, in:
		% [ slot_refused, slot_obtained, bad_login, bad_password,
		% account_not_active, already_connected, marshalling_failed, 
		% timed_out, access_granted, incompatible_version, normal_termination ]:
		status,
		
		% The user identifier is either an orge_user identifier, or undefined:
		user_identifier,
		
		% Login is the specified user login (if any):
		login,
		
		% Password is the specified user password (if any):
		password,
		
		% The IPv4 address of the peer {N1,N2,N3,N4}:
		ip_address,
		
		% The port of the peer is an unsigned integer:
		port,
		
		% The reverse DNS of the peer is an hostname (as a string) or the
		% 'unknown_dns' atom:
		reverse_dns,
		
		% The version triplet of the client used:
		client_version,
		
		% Connection start time is either a timestamp or undefined:
		start_time,
		
		% Connection stop time is either a timestamp or undefined:
		stop_time,
		
		% The supposed country of the peer, as geolocated:
		geolocated_country,
		
		% The supposed region of the peer, as geolocated:
		geolocated_region,
		
		% The supposed city of the peer, as geolocated:
		geolocated_city,
		
		% The supposed postal code of the peer, as geolocated:
		geolocated_postal_code
		
	}
).



% List of all the tables of interest in the Orge database:
-define( table_list, [ orge_user, orge_connection ] ).




% Starts an Orge database and returns its PID.
%  - InitMode is an atom that can be:
%   - from_scratch: to start a fresh new (blank) database
%   - from_previous_state: to start a previously existing database, read from
% file
%  - MaximumSlotCount is the maximum number of server slots 
% (simultaneous active connections)

start( InitMode, MaximumSlotCount ) ->
	spawn( fun() -> init( InitMode, MaximumSlotCount, no_listener ) end ).



% Starts a linked Orge database and returns its PID.
% See start/1 for parameter documentation.
start_link(InitMode) ->
	spawn_link( fun() -> init( InitMode, MaximumSlotCount, no_listener ) end ).



% Starts an Orge database, notifies a listener and returns the database PID. 
% Will notify specified listener whenever the database will be up and
% running.
% See start/1 for the documentation of other parameters.
start( InitMode, MaximumSlotCount, ListenerPid ) ->
	spawn( fun() -> init( InitMode, MaximumSlotCount, ListenerPid ) end ).



% Starts a linked Orge database, notifies a listener and returns the 
% database PID. 
% See start_link/1 for the documentation of other parameters.
start_link( InitMode, MaximumSlotCount, ListenerPid ) ->
	spawn_link( fun() -> init( InitMode, MaximumSlotCount, ListenerPid ) end ).



		
% Helper functions.


% init returns the first database state.
init( from_scratch, MaximumSlotCount, ListenerPid ) ->

	send_info( "Starting Orge database, created from scratch." ),
	
	start_geolocation_service(),
	
	% Includes only this node:
	DatabaseNodes = get_database_nodes(),
	
	% Ignore failure if no schema was already existing:
	case mnesia:delete_schema( DatabaseNodes ) of 
	
		ok ->
			send_info( "A previous database has been deleted." );

		{error,{Reason,Arg}} ->
			send_info( io_lib:format( 
				"No previous database deleted: ~s: ~w.",
				[Reason,lists:flatten(Arg)] ) );
		
		{error,Reason} ->
			send_info( io_lib:format( "No previous database deleted: ~w.",
				[Reason] ) )
	
	end,
	
	send_trace( "Creating database schema." ),
    ok = mnesia:create_schema( DatabaseNodes ),
	
	send_trace( "Starting database." ),
    ok = mnesia:start(),
	
	create_tables_on( DatabaseNodes ),
	notify_ready( ListenerPid, _InitialConnectionCount = 0 ),
	send_trace( "Orge database ready." ),
	
	% Current user ID is 1 here:
	loop( #database_state{ 
		current_user_id = 1,
		maximum_slot_count = MaximumSlotCount
	 } );
	
init( from_previous_state, MaximumSlotCount, ListenerPid ) ->	

	send_info( "Starting Orge database from previous state." ),
	
	start_geolocation_service(),

	ok = mnesia:start(),
	
	TargetTables = ?table_list,
	
 	send_debug( io_lib:format( 
		"Waiting for the loading of following tables: ~w.", [TargetTables] ) ),
		
	% Hangs until all tables in the list are accessible, or until the
	% time-out occurs (milliseconds):
	ok = mnesia:wait_for_tables( TargetTables, _timed_out = 5000 ),
	
	notify_ready( ListenerPid, get_highest_connection_number() ),
	
	send_trace( "Orge database ready." ),
	
	loop( #database_state{ 
		current_user_id = get_highest_user_number() + 1,
		maximum_slot_count = MaximumSlotCount
	} ).



notify_ready( no_listener, _InitialConnectionCount) ->
	ok;
	
notify_ready( ListenerPid, InitialConnectionCount ) ->
	ListenerPid ! {orge_database_ready,InitialConnectionCount}.



start_geolocation_service() ->

	case code:which(egeoip) of
	
		non_existing ->
			send_error(  "egeoip module not available, "
				"no IP geolocation can be performed." ),
			throw( {no_ip_geolocation,egeoip_not_found} );
		
		_Source ->	
			send_trace( "Starting IP geolocation service." ),
			{ok,_Pid} = egeoip:start(),
			send_info( "IP geolocation service successfully started." )
			
	end.
	
	
	
	
% The database main loop.
loop( DatabaseState ) ->

	send_debug( "Database waiting for requests." ),
	
	receive
	
	
		% User-related operations:

		{register_user,UserSettings,CallerPid} ->
			loop( register_user( DatabaseState,UserSettings,CallerPid ) );
			
		{unregister_user,UserLogin,CallerPid} ->
			loop( unregister_user( DatabaseState,UserLogin,CallerPid ) );



		% Connection-related operations, mostly requests sent by client
		% managers:

		{request_slot,ConnectionId,ClientNetId,ManagerPid} ->
			Answer = request_slot( ConnectionId, ManagerPid,ClientNetId,
				DatabaseState ),
			ManagerPid ! {ConnectionId,Answer},
			loop( DatabaseState );
								
		{try_login,{Login,Password},ConnectionId,ManagerPid} ->
			Answer = try_login(Login,Password,ConnectionId),
			ManagerPid ! {ConnectionId,Answer},
			loop( DatabaseState );
	
		{declare_marshalling_failed,ConnectionId,ClientNetId} ->
			record_marshalling_failure(ConnectionId,ClientNetId),
			loop( DatabaseState );
		
		{declare_timed_out,ConnectionId,ClientNetId} ->
			record_timed_out(ConnectionId,ClientNetId),
			loop( DatabaseState );
		
		{declare_incompatible_version,ConnectionId,ClientVersion} ->
			record_incompatible_version(ConnectionId,ClientVersion),
			loop( DatabaseState );

		{declare_normal_termination,ConnectionId} ->	
			record_normal_termination(ConnectionId),
			loop( DatabaseState );
	
		
		
		% Listing of informations.
		
		{list_all_users,CallerPid} ->
			CallerPid ! { orge_users, list_all_users() },
			loop( DatabaseState );

		{list_all_connections,CallerPid} ->
			CallerPid ! { orge_connections, list_all_connections() },
			loop( DatabaseState );
		
		{list_active_connection_identifiers,CallerPid} ->
			CallerPid ! { orge_active_connections,
				list_active_connection_identifiers() },
			loop( DatabaseState );
		
				
						
		% Operations on the database itself:

		{get_status,CallerPid} ->
			CallerPid ! { orge_database_status, get_status(DatabaseState) },
			loop( DatabaseState );
			
		reset ->
			loop( reset_tables(DatabaseState) );
				
		{stop,CallerPid} ->
			stop(DatabaseState,CallerPid);
		
		Other ->
			send_warning( io_lib:format( 
				"Database ignored following message: ~w.", [Other] ) )
	
	end.




% Section for user-related operations.



% Registers specified Orge user settings, where NewUserSettings is an 
% orge_user_settings record, in one transaction.
% The caller is notified of the result of the operation: user_registered or
% {user_registration_failed,AtomReason} is sent back, where AtomReason may be
% login_already_registered, write_transaction_failed, etc.
% Returns an updated state.
register_user( DatabaseState, NewUserSettings, CallerPid)  ->

	case check_validity( NewUserSettings, DatabaseState ) of 
	
		ok -> 
			% Validated, this is a new registering (not an update):
			ThisUserId = ?getState.current_user_id, 
			NewUser = update_from_settings( 
				#orge_user{  
					identifier = ThisUserId,
					account_creation_time = basic_utils:get_timestamp(),
					account_status = active,
					character_id_list = []
				}, NewUserSettings ),
				
			% Reduces the size in database:	
			BinarizedUser = binarize_user( NewUser ),
			
			F = fun() -> mnesia:write( BinarizedUser ) end,
			case mnesia:transaction(F) of
			
				{atomic,_} ->
					CallerPid ! user_registered,
					
					send_info( io_lib:format( "Registered a new user: ~s",
						[ user_to_string(NewUser) ] ) ),
						
					?getState{ current_user_id = ThisUserId + 1 };
				
				{aborted,FailReason} ->
					send_error( io_lib:format(
						"Registering of user settings (~s) failed: ~w",
						[user_settings_to_string(NewUserSettings), 
							FailReason] ) ),
					CallerPid !
						{user_registration_failed,write_transaction_failed},
					DatabaseState
					
			end;		

		RejectReason ->
			send_warning( io_lib:format(
				"Registering of user settings (~s) failed: ~w",
				[user_settings_to_string(NewUserSettings), RejectReason] ) ),
			CallerPid ! {user_registration_failed,RejectReason},
			DatabaseState
		
	end.	
	


% Unregisters an Orge user, specified by login. Its account will be marked as
% deleted.
% The caller is notified of the result of the operation: user_unregistered  or
% {user_unregistration_failed,AtomReason} is sent back, where AtomReason may be
% login_not_known, account_already_deleted, write_transaction_failed, etc.
% Returns an updated state.
unregister_user( DatabaseState, UserLogin, CallerPid ) ->

	case get_user_identifier_from_login( binarize_field(UserLogin) ) of
	
		not_found ->
			send_warning( io_lib:format(
				"Unregistering of user failed: unknown login '~s'",
				[UserLogin] ) ),
			CallerPid ! {user_unregistration_failed,login_not_known},
			DatabaseState;
		
		UserId ->
		
			F = fun() -> 
				CurrentUserInfos = mnesia:read( orge_user, UserId, write ),
				case CurrentUserInfos#orge_user.account_status of
			
					deleted ->
						account_already_deleted;

					_NonDeletedAccount ->
						UpdatedUserInfos = CurrentUserInfos#orge_user{
							account_deletion_time = basic_utils:get_timestamp(),
							account_status = deleted },
						mnesia:write( UpdatedUserInfos ),
						deletion_success
						
				end,	
			end,
			
			case mnesia:transaction(F) of
			
				{atomic,account_already_deleted} ->
					send_warning( io_lib:format(
						"Unregistering of user failed for login '~s': "
						"account already deleted", [UserLogin] ) ),
					CallerPid !
						{user_unregistration_failed,account_already_deleted},
					DatabaseState;
			
				{atomic,deletion_success} ->
					CallerPid ! user_unregistered,
					DatabaseState;
			
				{aborted,FailReason} ->
					send_error( io_lib:format(
						"Unregistering of user failed for login '~s': "
						"~w", [ UserLogin, FailReason ] ) ),
					CallerPid ! {user_unregistration_failed,
						write_transaction_failed},
					DatabaseState
			
			end
					
	end.




% User query section.
	
	

% Returns the list of all past and current users, regardless of their
% state.
list_all_users() ->
	list_table(orge_user).
	
	
	
% Returns the identifiers of all past and current users, regardless of their
% state.
list_all_user_identifiers() ->
	F = fun() ->
		MatchHead = #orge_user{ identifier = '$1', _ = '_'},
		Result = '$1',
		mnesia:select( orge_user, [ {MatchHead, _Guard=[], [Result]} ] ).
	end,
	{atomic,UserIdList} = mnesia:transaction(F),
	UserIdList.


% Determines from the user table what is the highest user
% identifier used.
% Note: useful when restoring the state of a database. 
get_highest_user_number() ->
	% Works even if no user was recorded, as starting at 0:
	case list_all_user_identifiers() of
	
		[] ->
			0;
			
		NonEmptyList ->
			lists:max( NonEmptyList )

	end.

	
	
	
% Connection query section.

	
% Returns the list of all past and current connections, regardless of their
% state.
list_all_connections() ->
	list_table(orge_connection).
	

	
% Returns the list of the identifiers of all known connections.
list_all_connection_identifiers() ->
	F = fun() ->
		MatchHead = #orge_connection{ identifier = '$1', _ = '_'},
		Result = '$1',
		mnesia:select( orge_connection, [ {MatchHead, _Guard=[], [Result]} ] ).
	end,
	{atomic,ConnectionIdList} = mnesia:transaction(F),
	ConnectionIdList.
	
	
	
% Determines from the connection table what is the highest connection
% identifier used.
% Note: useful when restoring the state of a database. 
get_highest_connection_number() ->
	% Works even if no connection was recorded, as starting at 0:
	case list_all_connection_identifiers() of
	
		[] ->
			0;
			
		NonEmptyList ->
			lists:max( NonEmptyList )

	end.


	
% Returns the list of identifier for all the connections fully accepted
% and running.
list_active_connection_identifiers() ->
	F = fun() ->
		MatchHead = #orge_connection{ identifier = '$1', 
			status = access_granted, _ = '_'},
		Result = '$1',
		mnesia:select( orge_connection, [ {MatchHead, _Guard=[], [Result]} ] ).
	end,
	{atomic,ActiveConnectionIdList} = mnesia:transaction(F),
	ActiveConnectionIdList.	
	%manage_query( qlc:q([ X #orge_connection.identifier 
	%	|| X <- mnesia:table(orge_connection),
	%	X#orge_connection.status =:= access_granted ]) ).
	
	
	
% Returns the current number of active connections (used slots).
get_active_connection_count() ->	
	length( list_active_connection_identifiers() ).



		
% Database-generic operation section.		
		
	
		
% Returns a textual description of the state of this database/	
get_status(DatabaseState) ->
	io_lib:format( "database running, the identifier that will be assigned "
		"to the next user is ~B, maximum slot count is ~B", 
		[ ?getState.current_user_id, ?getState.maximum_slot_count ] ).



% Manages generically a QLC query.	
manage_query(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic,Val} = mnesia:transaction(F),
    Val.
 
 
	
% Lists all entries of the specified table.
% Returns the entry list.
list_table(TableName) ->
	manage_query( qlc:q([ X || X <- mnesia:table(TableName) ]) ).
	%{atomic,TableRecords} = mnesia:select( _MatchHead = ..., etc.
	%TableRecords.



% Deletes all entries in the Orge tables.
reset_tables(DatabaseState) ->
	send_info( "Resetting tables of Orge database." ),
	lists:foreach( 
		fun(Table) -> 
			{atomic,ok} = mnesia:clear_table(Table)
		end,
		?table_list ),
	DatabaseState.



% Stops the database.
stop( _DatabaseState, CallerPid ) ->
	send_info( "Stopping Orge database." ),
    mnesia:stop(),
	CallerPid ! orge_database_stopped,
	send_trace( "Orge database stopped." ),
	
	

% Includes only this node for the moment:
get_database_nodes() ->
	[node()].
	
	


% Database prebuilt convenience queries.


% Returns either not_found, or the identifier of the user record 
% corresponding to the specified login. 	
get_user_identifier_from_login( AccountLogin ) ->
	
	F = fun() ->
		MatchHead = #orge_user{ identifier = '$1', 
			account_login = AccountLogin, _ = '_'},
		Result = '$1',
		mnesia:select( orge_user, [ {MatchHead, _Guard=[], [Result]} ] )
	end,
			
	case mnesia:transaction(F) of

	%case manage_query( qlc:q([ X.identifier || X <- mnesia:table(orge_user),
	%		X#orge_user.account_login =:= AccountLogin ]) ) of 
	
	   {atomic,[]} ->
		   not_found;
		   
	   % At most one entry should match:   
	   {atomic,[UserId]} ->
		   UserId    
		   
	end.



% Returns either not_connected, or the orge_connection record corresponding
% to specified Orge user identifier, supposed to be already connected. 	
get_running_connection_id_for( UserId ) ->

	F = fun() ->
		MatchHead = #orge_connection{ identifier = '$1', 
			status = access_granted, user_identifier = UserId, _ = '_' },
		Result = '$1',
		mnesia:select( orge_connection, [ {MatchHead, _Guard=[], [Result]} ] ).
	end,
	
	case mnesia:transaction(F) of
	%case manage_query( qlc:q([ X || X <- mnesia:table(orge_connection),
	%		X#orge_connection.user_identifier =:= UserId,
	%		X#orge_connection.status =:= access_granted ]) ) of 
		
		{atomic,[]} ->
			not_connected;
			
		% At most one entry should match:	
		{atomic,[ConnectionId]} ->
			ConnectionId	
			
	end	.
	


% Not possible, see below instead:
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

	send_trace( io_lib:format( "Creating tables on following node(s): ~w.",
		[DatabaseNodes] ) ),
	
	send_debug( "Creating table for Orge users." ),
	{atomic,ok} = mnesia:create_table( orge_user, [ 
		{attributes, record_info(fields,orge_user)},
		{disc_copies,DatabaseNodes},
		{type,set} ] ),
		
	send_debug( "Creating table for Orge connections." ),
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
		home_phone    = ?user_settings.home_phone,
		mobile_phone  =
			?user_settings.mobile_phone,
		email_address     = ?user_settings.email_address,
		account_login     = ?user_settings.account_login,
		account_password  = 
			?user_settings.account_password,
		security_question =
			?user_settings.security_question,
		security_answer   = ?user_settings.security_answer
	}.
	
	
	
% Returns an updated user settings record where fields which are strings are
% binaries instead of list of characters.
binarize_user( OrgeUser ) ->
	?user{
		first_name        = binarize_field( ?user.first_name ),
		last_name         = binarize_field( ?user.last_name ),
		date_of_birth     = binarize_field( ?user.date_of_birth ),
		address_line_1    = binarize_field( ?user.address_line_1 ),
		address_line_2    = binarize_field( ?user.address_line_2 ),
		city              = binarize_field( ?user.city ),
		state             = binarize_field( ?user.state ),
		country           = binarize_field( ?user.country ),
		postal_code       = binarize_field( ?user.postal_code ),
		home_phone        = binarize_field( ?user.home_phone ),
		mobile_phone      = binarize_field( ?user.mobile_phone ),
		email_address     = binarize_field( ?user.email_address ),
		account_login     = binarize_field( ?user.account_login ),
		account_password  = binarize_field( ?user.account_password ),
		security_question = binarize_field( ?user.security_question ),
		security_answer   = binarize_field( ?user.security_answer )
	}.
		


% Returns a translation of specified term targeting the storage in database.
binarize_field( Field ) ->
	case basic_utils:is_string(Field) of
	
		true -> 
			% The list must be a string:
			% Currently deactivated, as displayed as #Bin in tv application:
			%list_to_binary( Field );
			Field;
			
		false ->
			Field
			
	end.
	
	

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
			?user_settings.home_phone,
			?user_settings.mobile_phone,
			?user_settings.email_address,
			?user_settings.account_login,
			?user_settings.account_password,
			?user_settings.security_question,
			?user_settings.security_answer
			
		] ).



% Returns a textual description of specified Orge user record.
user_to_string(OrgeUser) ->

	DeleteString = case ?user.account_deletion_time of
	
		undefined ->
			"not deleted yet";
		
		DeleteTimestamp ->
			io_lib:format( "deleted on ~s", 
				[ basic_utils:timestamp_to_string(DeleteTimestamp) ] )	
	
	end,
	
	CharacterString = case ?user.character_id_list of
	
		[] ->
			"no registered character";
		
		CharacterList ->
			io_lib:format( 
				"following identifiers in terms of registered characters: ~w",
				[ CharacterList ] )	
	
	end,
	
	io_lib:format( "Account #~s created on ~s and ~s, "
		"with ~s. "
		"Current account status: ~s. "
		"User first name is '~s', last name is '~s', date of birth is '~s', "
		"first address line is '~s', second one is '~s', city is '~s', "
		"state is '~s', country is '~s', postal code is '~s', "
		"home telephone is '~s', mobile one is '~s', e-mail address is '~s', "
		" account login is '~s', account password is '~s', "
		"security question is '~s', security answer is '~s'",
		[ 
			basic_utils:integer_to_string( ?user.identifier ),
			basic_utils:timestamp_to_string( 
				?user.account_creation_time ),
			DeleteString,
			CharacterString,
			?user.character_id_list,
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
			?user.home_phone,
			?user.mobile_phone,
			?user.email_address,
			?user.account_login,
			?user.account_password,
			?user.security_question,
			?user.security_answer
		] ).




% Returns a textual description of specified Orge connection record.
% Note: no direct patter-matching on status to avoid to have to bind 
% specifically each field of interest.
connection_to_string(OrgeConnection) ->

	String = case OrgeConnection#orge_connection.status of
	
		slot_refused ->	
			connection_slot_refused_to_string(OrgeConnection);

		slot_obtained ->	
			connection_slot_obtained_to_string(OrgeConnection);
			 
		bad_login ->
			connection_bad_login_to_string(OrgeConnection);
			
		bad_password ->
			connection_bad_password_to_string(OrgeConnection);

		account_not_active ->
			connection_account_not_active_to_string(OrgeConnection);

		already_connected ->
			connection_already_connected_to_string(OrgeConnection);

		marshalling_failed ->
			connection_marshalling_failed_to_string(OrgeConnection);

		timed_out ->
			connection_timed_out_to_string(OrgeConnection);
		
		access_granted ->
			connection_access_granted_to_string(OrgeConnection);
		
		incompatible_version ->
			connection_incompatible_version_to_string(OrgeConnection);
			
		normal_termination ->
			connection_normal_termination_to_string(OrgeConnection)
			
	end,
	String ++ " " ++ geolocation_to_string(OrgeConnection).
	
	




% Section about textual descriptions of connections.	



connection_slot_refused_to_string(OrgeConnection) ->
	io_lib:format( "Connection #~s refused by the server "
		"due to a lack of available slot at ~s from host ~s whose ~s.",
		[ 
			basic_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time ),
			basic_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port	),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns )
		] ).
		
		
		
connection_slot_obtained_to_string(OrgeConnection) ->
	io_lib:format( "Connection #~s obtained a server slot "
		"at ~s from host ~s whose ~s.",
		[ 
			basic_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time ),
			basic_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port	),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns )
		] ).



connection_bad_login_to_string(OrgeConnection) ->
	io_lib:format( "Failed connection #~s due to bad login '~s' "
		"(with specified password '~s') from host ~s whose ~s at ~s.",
		[ 
			basic_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.password,		
			basic_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time )
		] ).



connection_bad_password_to_string(OrgeConnection) ->
	io_lib:format( "Failed connection #~s due to bad password '~s' "
		"for known login '~s' from host ~s whose ~s at ~s.",
		[ 
			basic_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			OrgeConnection#orge_connection.password,		
			OrgeConnection#orge_connection.login,
			basic_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time )
		] ).



connection_account_not_active_to_string(OrgeConnection) ->
	io_lib:format( "Failed connection #~s due to a non-active account"
		" for Orge user #~s (login: '~s', password: '~s'), "
		"from host ~s whose ~s, at ~s.",
		[ 
			basic_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			basic_utils:integer_to_string(
				OrgeConnection#orge_connection.user_identifier ),
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.password,		
			basic_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time )
		] ).
		
		
		
connection_already_connected_to_string(OrgeConnection) ->
	io_lib:format( "Failed connection #~s due to an already existing connection"
		" for Orge user #~s (login: '~s', password: '~s'), "
		"from host ~s whose ~s, at ~s.",
		[ 
			basic_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			basic_utils:integer_to_string(
				OrgeConnection#orge_connection.user_identifier ),
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.password,		
			basic_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time )
		] ).



connection_marshalling_failed_to_string(OrgeConnection) ->
	io_lib:format( "Failed marshalling for connection #~s at ~s "
		"from host ~s whose ~s.",
		[ 
			basic_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time ),
			basic_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port	),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns )
		] ).



connection_timed_out_to_string(OrgeConnection) ->
	io_lib:format( "Time-out for connection #~s at ~s from host ~s whose ~s.",
		[ 
			basic_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time ),
			basic_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port	),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns )
		] ).



connection_access_granted_to_string(OrgeConnection) ->
	StopSentence = case OrgeConnection#orge_connection.stop_time of 
	
		undefined ->
			"still active";
		
		Timestamp ->
			io_lib:format( "stopped at ~s", 
				[ basic_utils:timestamp_to_string(Timestamp) ] )
	
	end,
	io_lib:format( "Successful connection #~s of Orge user #~s "
		"(login: '~s', password: '~s') from host ~s whose ~s, "
		"started at ~s, ~s.",
		[ 
			basic_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			basic_utils:integer_to_string(
				OrgeConnection#orge_connection.user_identifier ),
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.password,
			basic_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time ),
			StopSentence
		] ).
						


connection_incompatible_version_to_string(OrgeConnection) ->
	io_lib:format( "Connection #~s was terminated normally "
		"for Orge user #~s (login: '~s', password: '~s'), "
		"from host ~s whose ~s, started at ~s, ended at ~s.",
		[ 
			basic_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			basic_utils:integer_to_string(
				OrgeConnection#orge_connection.user_identifier ),
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.password,		
			basic_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.stop_time )
		] ).



connection_normal_termination_to_string(OrgeConnection) ->
	io_lib:format( "Connection #~s was terminated normally "
		"for Orge user #~s (login: '~s', password: '~s'), "
		"from host ~s whose ~s, started at ~s, ended at ~s.",
		[ 
			basic_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			basic_utils:integer_to_string(
				OrgeConnection#orge_connection.user_identifier ),
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.password,		
			basic_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.stop_time )
		] ).




% Returns a textual description of the login status.
	
status_to_string(access_granted) ->
	"successfully logged-in";
	
status_to_string(bad_login) ->
	"failed login due to bad login";
	
status_to_string(bad_password) ->
	"failed login due to bad password";
	
status_to_string(timed_out) ->
	"failed login due to client time-out";
	
status_to_string(slot_refused) ->
	"failed connection short of having one available server slot";
	
status_to_string(slot_obtained) ->
	"connection which obtained a server slot";
	
status_to_string(marshalling_failed) ->
	"failed login due to client ill-formatted identifier data";
	
status_to_string(already_connected) ->
	"failed login since user already logged-in";
		
status_to_string(account_not_active) ->
	"failed login since user account not active".
	
		

% Returns a textual description of the reverse DNS.
reverse_dns_to_string( unknown_dns ) ->
	"IP address could not be resolved in the DNS";
			
reverse_dns_to_string( DNSName ) ->
	io_lib:format( "IP address is resolved to '~s'", [DNSName] ).
	
	

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
	




% Section for the validity checking of user information 
% (on initial registration).



% First check, avoids too much nesting.
check_validity( NewUserSettings, DatabaseState ) ->
	case check_validity_of_personal_infos(
			NewUserSettings#orge_user_settings.first_name,
			NewUserSettings#orge_user_settings.last_name,
			NewUserSettings#orge_user_settings.date_of_birth ) of 
		
		ok ->
			address_validity_check( NewUserSettings,DatabaseState );
				
		Other ->
			Other			
	
	end.
	
	
	
address_validity_check(NewUserSettings,DatabaseState) ->
	case check_validity_of_actual_address(
			NewUserSettings#orge_user_settings.address_line_1,
			NewUserSettings#orge_user_settings.address_line_2,
			NewUserSettings#orge_user_settings.city,
			NewUserSettings#orge_user_settings.state,
			NewUserSettings#orge_user_settings.country,
			NewUserSettings#orge_user_settings.postal_code ) of 
					
		ok ->
			phone_validity_check( NewUserSettings,DatabaseState );	
	
		Other ->
			Other			
				
	end.



phone_validity_check(NewUserSettings,DatabaseState) ->
	case check_validity_of_phones(
			NewUserSettings#orge_user_settings.home_phone,
			NewUserSettings#orge_user_settings.mobile_phone	) of 
	
		ok ->
			mail_validity_check( NewUserSettings,DatabaseState );	
					
		Other ->
			Other			

	end.
	


mail_validity_check(NewUserSettings,DatabaseState) ->
	case check_validity_of_email(
			NewUserSettings#orge_user_settings.email_address ) of 
	
		ok ->
			account_validity_check( NewUserSettings,DatabaseState );	
					
		Other ->
			Other			

	end.
	
	
	
account_validity_check(NewUserSettings,DatabaseState) ->
	case check_validity_of_account(
			NewUserSettings#orge_user_settings.account_login,
			NewUserSettings#orge_user_settings.account_password	) of 
	
		ok ->
			security_validity_check( NewUserSettings,DatabaseState );	
					
		Other ->
			Other			

	end.
	
	
	
security_validity_check(NewUserSettings,_DatabaseState) ->
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
	case get_user_identifier_from_login( AccountLogin ) of 
	
		not_found ->
			ok;
			
		_Other ->
			login_already_registered	
	
	end.
	
	
	
check_validity_of_security_challenge( _SecurityQuestion, _SecurityAnswer ) ->
	% TO-DO
	ok.
	
	
	


% Section for the validity checking of a user login.



% Tells whether at least one connection slot is available and, if yes,
% reserves it.
request_slot( ConnectionId, ManagerPid, ClientNetId, DatabaseState ) ->

	MaxCount = ?getState.maximum_slot_count,
	
	case get_active_connection_count() of 
	
		MaxCount ->
			record_slot_refused( ConnectionId, ClientNetId );
			
		_LessThanMaxCount ->
			record_slot_obtained( ConnectionId, ClientNetId )
	
	end.



% Checks specified login request, logs it appropriately in database and 
% performs the login if successful.
try_login( StringLogin, SentPassword, ConnectionId ) ->

	Login = binarize_field(StringLogin),
	
	% First, check that login is known and that password matches:
	case get_user_identifier_from_login( Login ) of
	
		not_found ->
			record_bad_login( Login, SentPassword, ConnectionId );
		
		UserId ->

			% Gets the corresponding user record:
			F = fun() ->
				mnesia:read( UserId )
			end,	
			
			case mnesia:transaction(F) of

				% Useless to match also with Login.
				% At most one record should match:   
				{atomic, [ #orge_user{ account_status = AccountStatus, 
						account_password = SentPassword } ] } ->
					% Matching here with the already bound SentPassword:
					case AccountStatus of 
			
						active ->
			
							% Login/password ok, account active, check 
							% this user is not already connected currently:
							case get_running_connection_id_for(UserId) of
			
								not_connected ->
									% OK, connection can then proceed:
									record_access_granted( Login, SentPassword,
										ConnectionId, UserId );
													
								OtherConnectionId ->
									% Failure, as already connected:
									record_already_connected( Login,
										ConnectionId, OtherConnectionId, UserId)
					
							end;
			
						OtherAccountStatus ->
							record_account_non_active( Login, SentPassword,
								ConnectionId, OtherAccountStatus, UserId )
					
					end;
					
				{atomic, [ #orge_user{ 
						account_password = CorrectPassword } ] } ->
					% We have here a match, but with a different password:
					record_bad_password( Login, SentPassword, CorrectPassword,
						ConnectionId )

			end,		
		   
	end.
		
				


% Section to record state changes in the connection.
% Note: the two slot record functions are where the connection entry is created.


% Creates a connection entry for that initial slot request, which was refused.
record_slot_refused( ConnectionId, {ClientIP,ClientPort,ReversedDNS} ) ->

	% Creating the new connection entry with first information:
	NewConnection = #orge_connection{
		identifier = ConnectionId,
		status = slot_refused,
		ip_address = ClientIP,
		port = ClientPort,
		reverse_dns = ReversedDNS,
		start_time = basic_utils:get_timestamp(),
		stop_time  = basic_utils:get_timestamp(),
	},
	
	LocatedConnection = add_geolocation_infos(NewConnection),
	
	F = fun() -> mnesia:write( LocatedConnection ) end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			send_info( io_lib:format( "Slot refused for connection #~B.",
				[ConnectionId] ) ),
			slot_refused;
							
		{aborted,FailReason} ->
			send_error( io_lib:format(
				"Registering of slot rejection for connection (~s) failed: ~w",
					[connection_to_string(NewConnection),FailReason] ) ),
			internal_error	
	
	end.	
	
	
	
% Creates a connection entry for that initial slot request, which was accepted.
% Note: this is where the connection entry is created.
record_slot_obtained( ConnectionId, {ClientIP,ClientPort,ReversedDNS} ) ->

	% Creating the new connection entry with first information:
	NewConnection = #orge_connection{
		identifier = ConnectionId,
		status = slot_obtained,
		ip_address = ClientIP,
		port = ClientPort,
		reverse_dns = ReversedDNS,
		start_time = basic_utils:get_timestamp()
	},
	
	LocatedConnection = add_geolocation_infos(NewConnection),
	
	F = fun() -> mnesia:write( LocatedConnection ) end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			send_info( io_lib:format( "Slot obtained for connection #~B.",
				[ConnectionId] ) ),
			slot_obtained;
							
		{aborted,FailReason} ->
			send_error( io_lib:format(
				"Registering of slot obtainment for connection (~s) failed: ~w",
				[connection_to_string(NewConnection),FailReason] ) ),
			internal_error	
	
	end.	



% Records that connection and returns an atom describing its status.
record_access_granted( Login, Password, ConnectionId, UserId ) ->

	% Just updating the corresponding connection entry:		
	F = fun() -> 
			CurrentConnection = mnesia:read( orge_connection, ConnectionId,
				write ),
			UpdatedConnection = CurrentConnection#orge_connection{
				status = access_granted,
				user_identifier = UserId,
				login = Login,
				password = Password
			},
			mnesia:write( UpdatedConnection ) 
		
		end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			send_info( io_lib:format( "Access granted for ~s "
				"(connection #~B).", [Login,ConnectionId] ) ),
			access_granted;
							
		{aborted,FailReason} ->
			send_error( io_lib:format(
				"Registering of granted connection (~s) failed: ~w",
					[connection_to_string(NewConnection),FailReason] ) ),
			internal_error	
	
	end.	



% Records that connection and returns an atom describing its status.
record_bad_login( Login, Password, ConnectionId ) ->

	% Just updating the corresponding connection entry:		
	F = fun() -> 
			CurrentConnection = mnesia:read( orge_connection, ConnectionId,
				write ),
			UpdatedConnection = CurrentConnection#orge_connection{
				status = bad_login,
				login = Login,
				password = Password,
				stop_time = basic_utils:get_timestamp()
			},
			mnesia:write( UpdatedConnection ) 
		
		end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			send_warning( io_lib:format( "Bad login ('~s') specified, "
				"with password '~s' (connection #~B).", 
				[ Login, Password, ConnectionId ] ) ),
			bad_login;
							
		{aborted,FailReason} ->
			send_error( io_lib:format(
				"Registering of connection with bad login (~s) failed: ~w",
					[connection_to_string(NewConnection),FailReason] ) ),
			internal_error	
	
	end.	
	
	
	
% Records that connection and returns an atom describing its status.
record_bad_password( Login, WrongPassword, CorrectPassword, ConnectionId ) ->

	% Just updating the corresponding connection entry:		
	F = fun() -> 
			CurrentConnection = mnesia:read( orge_connection, ConnectionId,
				write ),
			UpdatedConnection = CurrentConnection#orge_connection{
				status = bad_password,
				login = Login,
				password = WrongPassword,
				stop_time = basic_utils:get_timestamp()
			},
			mnesia:write( UpdatedConnection ) 
		
		end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			send_warning( io_lib:format( "Bad password ('~s') specified "
				"for login '~s' instead of password '~s' (connection #~B).", 
				[ WrongPassword, CorrectPassword, Login, ConnectionId ] ) ),
			bad_password;
							
		{aborted,FailReason} ->
			send_error( io_lib:format(
				"Registering of connection with bad password (~s) failed: ~w",
					[connection_to_string(NewConnection),FailReason] ) ),
			internal_error	
	
	end.	


	
% Records that connection and returns an atom describing its status.
record_timed_out( ConnectionId ) ->
	
	% Just updating the corresponding connection entry:		
	F = fun() -> 
			CurrentConnection = mnesia:read( orge_connection, ConnectionId,
				write ),
			UpdatedConnection = CurrentConnection#orge_connection{
				status = timed_out,
				stop_time = basic_utils:get_timestamp()
			},
			mnesia:write( UpdatedConnection ) 
		
		end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			send_warning( io_lib:format( "Connection time-out for ~p "
				"(connection #~B).", [ ClientIP, ConnectionId ] ) ),
			timed_out;
							
		{aborted,FailReason} ->
			send_error( io_lib:format(
				"Registering of connection with time-out (~s) failed: ~w",
					[connection_to_string(NewConnection),FailReason] ) ),
			internal_error	
	
	end.	



% Records that connection and returns an atom describing its status.
% Garbled identifiers could be stored maybe.
record_marshalling_failure(ConnectionId ) ->
	NewConnection = #orge_connection{
		identifier = ConnectionId,
		status = marshalling_failed,
		%user_identifier
		%login
		%password
		ip_address = ClientIP,
		port = ClientPort,
		reverse_dns = ReversedDNS,
		start_time = basic_utils:get_timestamp()
		%stop_time
	},
	
	LocatedConnection = add_geolocation_infos(NewConnection),
	
	F = fun() -> mnesia:write( LocatedConnection ) end,
	% Just updating the corresponding connection entry:		
	F = fun() -> 
			CurrentConnection = mnesia:read( orge_connection, ConnectionId,
				write ),
			UpdatedConnection = CurrentConnection#orge_connection{
				client_version = ClientVersion,
				stop_time = basic_utils:get_timestamp()
			},
			mnesia:write( UpdatedConnection ) 
		
		end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			send_warning( io_lib:format( "Marshalling failed for ~p "
				"(connection #~B).", [ ClientIP, ConnectionId ] ) ),
			marshalling_failed;
							
		{aborted,FailReason} ->
			send_error( io_lib:format(
				"Registering of connection with marshalling failure "
				"(~s) failed: ~w",
				[connection_to_string(NewConnection),FailReason] ) ),
			internal_error	
	
	end.	



% Records that connection and returns an atom describing its status.
record_incompatible_version( ConnectionId, ClientVersion ) ->

	% Just updating the corresponding connection entry:		
	F = fun() -> 
			CurrentConnection = mnesia:read( orge_connection, ConnectionId,
				write ),
			UpdatedConnection = CurrentConnection#orge_connection{
				status = timed_out,
				client_version = ClientVersion,
				stop_time = basic_utils:get_timestamp()
			},
			mnesia:write( UpdatedConnection ) 
		
		end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			send_debug( io_lib:format( 
				"Connection with client version ~s recorded "
				"(connection #~B).", [ 
					basic_utils:version_to_string(ClientVersion), 
					ConnectionId ] ) ),
			ok;
							
		{aborted,FailReason} ->
			send_error( io_lib:format(
				"Registering of client version for connection (~s) failed: ~w",
				[connection_to_string(UpdatedClientConnection),FailReason] ) ),
			internal_error	
	
	end.	


	
% Records that connection and returns an atom describing its status.
record_already_connected( Login, ConnectionId, OtherConnectionId, UserId ) ->
		
	NewConnection = #orge_connection{
		identifier = ConnectionId,
		status = already_connected,
		user_identifier = UserId,
		login = Login,
		password = Password,
		ip_address = ClientIP,
		port = ClientPort,
		reverse_dns = ReversedDNS,		
		start_time = basic_utils:get_timestamp()
		%stop_time
	},
	
	LocatedConnection = add_geolocation_infos(NewConnection),
	
	F = fun() -> mnesia:write( LocatedConnection ) end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			send_warning( io_lib:format( 
				"Connection #~B from user ~B ('~s') disallowed, "
				"as is already connected as connection #~B.", 
				[ ConnectionId, UserId, Login, OtherConnectionId ] ) ),
			already_connected;
							
		{aborted,FailReason} ->
			send_error( io_lib:format( 
				"Registering of already existing connection (~s) failed: ~w",
				[connection_to_string(NewConnection),FailReason] ) ),
			internal_error	
	
	end.	
	


% Records that connection and returns an atom describing its status.
record_account_non_active( Login, Password,	ConnectionId, AccountStatus,
		UserId ) ->
		
	NewConnection = #orge_connection{
		identifier = ConnectionId,
		status = account_not_active,
		user_identifier = UserId,
		login = Login,
		password = Password,
		ip_address = ClientIP,
		port = ClientPort,
		reverse_dns = ReversedDNS,		
		start_time = basic_utils:get_timestamp()
		%stop_time
	},
	
	LocatedConnection = add_geolocation_infos(NewConnection),
	
	F = fun() -> mnesia:write( LocatedConnection ) end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			send_warning( io_lib:format( "Connection #~B from ~p disallowed, "
				"as the account of user '~s' is not active (status: ~s).", 
				[ ConnectionId, Login, AccountStatus ] ) ),
			account_not_active;
							
		{aborted,FailReason} ->
			send_error( io_lib:format( 
				"Registering of connection failed due to a non-active account "
				"(~s) failed: ~w",
				[connection_to_string(NewConnection),FailReason] ) ),
			internal_error	
	
	end.	
	


	
% Updates that connection and returns an atom describing its status.
record_normal_termination(ConnectionId) ->

	% Checks that session exists and is recorded as active:
	Connection = get_active_connection(ConnectionId),
	
	% Updates its entry (no status change):
	NewConnection = Connection#orge_connection { 
		stop_time = basic_utils:get_timestamp()
	},
	
	LocatedConnection = add_geolocation_infos(NewConnection),
	
	F = fun() -> mnesia:write( LocatedConnection ) end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			send_trace( io_lib:format( "Connection #~B ended.", 
				[ ConnectionId ] ) ),
			normal_termination;
							
		{aborted,FailReason} ->
			send_error( io_lib:format( 
				"Updating of ended connection (~s) failed: ~w",
				[connection_to_string(NewConnection),FailReason] ) ),
			internal_error	
	
	end.	
	


% Updates connection records with IP geolocation informations.
add_geolocation_infos(Connection) ->

	TargetAddress = basic_utils:ipv4_to_string( 
		Connection#orge_connection.ip_address ),
		
	case egeoip:lookup( TargetAddress ) of
	
		{ ok, {geoip,_CountryCode, _OtherCountryCode, CountryName, Region, City,
				PostalCode, _Latitude, _Longitude, _AreaCode, _DMACode } } ->
			Connection#orge_connection{
				geolocated_country = CountryName,
				% Unless binarize_field is a no-op, will perform a useless
				% double conversion:
				geolocated_region = binarize_field( binary_to_list(Region) ),
				geolocated_city = binarize_field( binary_to_list(City) ),
				geolocated_postal_code = binarize_field(
					binary_to_list(PostalCode) )
			};
			
		{ error, Reason } ->
			send_error( io_lib:format( "Geolocation of ~s failed: ~w",
				[TargetAddress,Reason] ) ),
			% Returning as is:	
			Connection
			
	end.
	
	
					
		
% Trace section.

% Note: can be factorized in a header or in a module due to the
% use of the defines.

-define( trace_emitter_name, "Database" ).
-define( trace_emitter_categorization, "Orge" ).


%send_fatal( Message ) ->
%	?notify_fatal( Message, ?trace_emitter_name, 
%		?trace_emitter_categorization ).

	
send_error( Message ) ->
	?notify_error( Message, ?trace_emitter_name, 
		?trace_emitter_categorization ).
	
	
send_warning( Message ) ->
	?notify_warning( Message, ?trace_emitter_name, 
		?trace_emitter_categorization ).
	
	
send_info( Message ) ->
	?notify_info( Message, ?trace_emitter_name, 
		?trace_emitter_categorization ).
	
	
send_trace( Message ) ->
	?notify_trace( Message, ?trace_emitter_name, 
		?trace_emitter_categorization ).
	
	
send_debug( Message ) ->
	?notify_debug( Message, ?trace_emitter_name, 
		?trace_emitter_categorization ).


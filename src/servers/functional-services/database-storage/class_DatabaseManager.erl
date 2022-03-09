% Copyright (C) 2003-2013 Olivier Boudeville
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
%
% Stores most of the main persistent information regarding connections, users,
% and simulated elements.
-module(class_DatabaseManager).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [class_TraceEmitter] ).


% Parameters taken by the constructor ('construct'). 
-define( wooper_construct_parameters, InitMode, MaximumSlotCount, ListenerPid ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3, 
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronous_timed_new/4,
		 remote_synchronous_timed_new_link/4, construct/4, delete/1 ).


% Member method declarations.
-define( wooper_method_export, registerUser/2, unregisterUser/2,
		 requestSlot/3, tryLogin/4, 
		 declareMarshallingFailed/2, declareTimedOut/2, 
		 declareIncompatible_version/3, declareShutdownByServer/2, 
		 declareUnexpectedClientTermination/2, 
		 declareNormalClientSideTermination/2, 
		 listAllUsers/1, listAllConnections/1, 
		 listAllActiveConnectionIdentifiers/1,
		 getStatus/1, backup/1, resetTables/1, stop/1 ).


-export([ user_settings_to_string/1, user_to_string/1, status_to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Necessary to call QLC query language functions:
-include_lib("stdlib/include/qlc.hrl").


% For orge_user_settings and al:
-include("class_DatabaseManager.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Orge.DatabaseManagement").


% For trace macros:
-include("class_TraceEmitter.hrl").



% Implementation notes.

% This database manager uses Mnesia to store the information specified in
% OrgeDatabase.rst.
%
% To each table (ex: user_settings) corresponds a dedicated record definition,
% and the two are named identically (useful with mnesia:write/1, for example).
%
% The first attribute of the record is always the primary key, most of the time
% chosen to be an arbitrary identifier (more precisely: a counter).
%
% Accesses to the database will mostly be transaction-based.
%
% The user table (orge_user) will be created in RAM and on disk as well.
%
% The connection table (orge_connection) will be created in RAM and on disk as
% well.
%
% Therefore, if the Orge database manager runs on a 32-bit VM, the maximum size
% for a table is currently 4GB (4e9 bytes), whereas on a 64-bit VM it is 16
% exabytes (16e18 bytes).

% mnesia:read, mnesia:select and qlc are preferred in this decreasing order,
% according to
% http://erlang.org/pipermail/erlang-questions/2009-January/040922.html

% We use disc_copies, so that a replica of the table resides both in RAM and on
% disc. Write operations addressed to the table will address both the RAM and
% the disc copy of the table, whereas read operations will just involve the RAM,
% for increased performances.

% Passwords are stored in the database not as plain text, but as encrypted
% hashes (now sha, was md5, could be hmac, des, aes, rsa, dss, etc.). 

% It requires a working crypto module, hence the generation of an Erlang VM
% linking to OpenSSL, which can be done quite easily, including with our
% install-erlang.sh script.





% Shortcut macros, for convenience: 

-define(user_settings,OrgeUserSettings#orge_user_settings).
-define(user,OrgeUser#orge_user).



% Describes the Mnesia internal entry type (fields) for Orge users:
% (corresponds to a non-nested orge_user_settings record, plus other fields).
% A Orge user can only have one Orge account.
% The identifier is a counter set by the database manager, starting from 1.
-record( orge_user, 
	{
	
		% Internal fields (i.e. they are not orge_user_settings fields):
		
		% The identifier of that user (unsigned integer); this is a counter set
		% by the database manager, starting from 1:
		identifier,
		
		% Role of this user, in: [ normal_user, admin ]:
		role,
		
		% Comes from orge_user_settings: 
		account_login,
		
		% Current status of the account, in [active,suspended,deleted]:
		account_status,

		% Timestamp corresponding to the creation time of this account:
		account_creation_time,

		% Timestamp corresponding to the deletion time of this account:
		account_deletion_time,		

		% List of the identifiers of the characters the user can control: 
		character_id_list,

		% Comes from orge_user_settings: 
		% (can be actually the hash of the real password)
		account_password,
		
		% orge_user_settings expanded fields:
		% account_login and account_password have been moved above to increase
		% the readability of table entries.
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
		security_question,
		security_answer
		
	}
).



% Describes an Orge character, i.e. a creature that can be controlled by a
% player.
%
%-record( orge_character, 
%	{
%
%		name,
%		
%		% Character record: flags telling whether the controlling player
%		% authorized this character to be reinjected in-game, as NPC or monster.
%		% Each character should have a textual physical and psychological
%		% description, of a few lines.
%		reuse
%		
%	}
%).



% Describes the Mnesia internal entry type (fields) for an Orge connection.
% Note: depending on the status, some fields will be left to undefined.
-record( orge_connection, 
	{
	
		% The connection identifier is a counter set by the Orge server,
		% starting from 1:
		identifier,
		
		% Current status of the connection, in [ slot_refused, slot_obtained,
		% bad_login, bad_password, account_not_active, already_connected,
		% marshalling_failed, timed_out, access_granted, incompatible_version,
		% shutdown_by_server, unexpected_client_termination,
		% normal_client_side_termination ]:
		status,
		
		% The user identifier is either an orge_user identifier, or undefined:
		user_identifier,
		
		% Login is the specified user login (if any):
		login,
		
		% Password is the specified user password (if any), or its hash:
		password,

		% The version triplet of the client used:
		client_version,
		
		% Connection start time is either a timestamp or undefined:
		start_time,
		
		% Connection stop time is either a timestamp or undefined:
		stop_time,
		
		% The IPv4 address of the peer {N1,N2,N3,N4}:
		ip_address,
		
		% The port of the peer is an unsigned integer:
		port,
		
		% The reverse DNS of the peer is an hostname (as a string) or the
		% 'unknown_dns' atom:
		reverse_dns,
				
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




% Constructs a new database manager.
% Starts an Orge database, with following parameters:
%  - InitMode is an atom that can be:
%   - from_scratch: to start a fresh new (blank) database
%   - from_previous_state: to start a previously existing database, read from
% file
%  - MaximumSlotCount is the maximum number of server slots 
%  - ListenerPid allows to specify a listener for that database manager, unless
%  'no_listener' is specified instead of a PID
%
%
% Attribute description:
% 
% - current_user_id is the identifier that will be assigned to the next new user
%
% - maximum_slot_count is the maximum number of server slots (simultaneous
% active connections)
% 
construct( State, _InitMode=from_scratch, MaximumSlotCount, ListenerPid ) ->

	% First the direct mother classes:
	TraceState = class_TraceEmitter:construct( State, 
											   "Main Orge Database Manager" ),
	
	?send_info( TraceState, "Starting Orge database, created from scratch." ),
	
	% To be enabled, if passwords are hashed:
	crypto:start(),
	
	start_geolocation_service( TraceState ),
	
	% Includes currently only this node:
	DatabaseNodes = get_database_nodes(),
	
	% Ignore failure if no schema was already existing:
	case mnesia:delete_schema( DatabaseNodes ) of 
	
		ok ->
			?send_info( TraceState, "A previous database has been deleted." );

		{error,{Reason,Arg}} ->
			?send_info_fmt( TraceState, "No previous database deleted: ~s: ~w.",
				[Reason,lists:flatten(Arg)] );
		
		{error,Reason} ->
			?send_info_fmt( TraceState, "No previous database deleted: ~w.", 
							[Reason] )
	
	end,
	
	?send_trace( TraceState, "Creating database schema." ),
    ok = mnesia:create_schema( DatabaseNodes ),
	
	?send_trace( TraceState, "Starting database." ),
    ok = mnesia:start(),
	
	create_tables_on( DatabaseNodes, TraceState ),
	notify_ready( ListenerPid, _InitialConnectionCount = 0 ),

	?send_trace( TraceState, "Orge database ready." ),
	
	% Then the class-specific attributes (of course almost all the state is in
	% the tables):
	setAttributes( TraceState, [
			{current_user_id,1},
			{maximum_slot_count,MaximumSlotCount},
		    {trace_categorization,
			 text_utils:string_to_binary(?TraceEmitterCategorization)}
							   ]);

construct( State, _InitMode=from_previous_state, MaximumSlotCount, 
		   ListenerPid ) ->

	% First the direct mother classes:
	TraceState = class_TraceEmitter:construct( State,
											   "Main Orge Database Manager" ),

	?send_info( TraceState, "Starting Orge database from previous state." ),
	
	start_geolocation_service( TraceState ),

	ok = mnesia:start(),
	
	TargetTables = ?table_list,
	
 	?send_debug_fmt( TraceState, 
					 "Waiting for the loading of following tables: ~w.", 
					 [TargetTables] ),
		
	% Hangs until all tables in the list are accessible, or until the time-out
	% occurs (milliseconds):
	ok = mnesia:wait_for_tables( TargetTables, _timed_out = 5000 ),
	
	notify_ready( ListenerPid, get_highest_connection_number() ),
	
	?send_trace( TraceState, "Orge database ready." ),
	
	setAttributes( TraceState, [
			{current_user_id,get_highest_user_number() + 1},
			{maximum_slot_count,MaximumSlotCount},
		    {trace_categorization,
			 text_utils:string_to_binary(?TraceEmitterCategorization)}
							   ]).



delete(State) ->
	?trace( "Deleting database manager."  ),
	% Do something 
	State.
	





		
% Helper functions.


notify_ready( no_listener, _InitialConnectionCount) ->
	ok;
	
notify_ready( ListenerPid, InitialConnectionCount ) ->
	ListenerPid ! {orge_database_ready,InitialConnectionCount}.



% Starts the service of IP geolocation of the addresses of Orge clients.
start_geolocation_service( State ) ->

	case code:which(egeoip) of
	
		non_existing ->
			?error( "egeoip module not available, "
				"no IP geolocation can be performed." ),
			throw( {no_ip_geolocation,egeoip_not_found} );
		
		_Source ->	
			?trace( "Starting IP geolocation service." ),
			{ok,_Pid} = egeoip:start(),
			?info( "IP geolocation service successfully started." )
			
	end.
	
	



% Method section.



% User-related operations.



% Registers specified user.
%
% Registers specified Orge user settings, where NewUserSettings is an
% orge_user_settings record, in one transaction.
%
% The caller is notified of the result of the operation: user_registered or
% {user_registration_failed,AtomReason} is sent back, where AtomReason may be
% login_already_registered, write_transaction_failed, etc.
%
% (request)
registerUser( State, NewUserSettings )  ->

	case check_validity( NewUserSettings, State ) of 
	
		ok -> 
			% Validated, this is a new registering (not an update):
			ThisUserId = ?getAttr(current_user_id), 
			NewUser = update_from_settings( 
				#orge_user{  
					identifier = ThisUserId,
					role = normal_user,
					account_creation_time = basic_utils:get_timestamp(),
					account_status = active,
					character_id_list = []
				}, NewUserSettings ),
			
			% We prefer to store hashes of passwords rather than the passwords
			% themselves:
			HashedPassword = get_hash( NewUser#orge_user.account_password ),
			
			HashedUser = NewUser#orge_user{ account_password = HashedPassword },
			
			% Reduces the size in database:	
			BinarizedUser = binarize_user( HashedUser ),
			
			F = fun() -> mnesia:write( BinarizedUser ) end,
			case mnesia:transaction(F) of
			
				{atomic,_} ->
					
					?info_fmt( "Registered a new user: ~s",
						[ user_to_string(NewUser) ] ),
						
					?wooper_return_state_result( 
					   setAttribute( State, current_user_id, ThisUserId + 1),
					   user_registered );
							
	
				{aborted,FailReason} ->
					?error_fmt( "Registering of user settings (~s) failed: ~w",
						[user_settings_to_string(NewUserSettings),FailReason] ),

					?wooper_return_state_result( State, 
						 {user_registration_failed,write_transaction_failed})
					
			end;		

		RejectReason ->
			?warning_fmt( "Registering of user settings (~s) failed: ~w",
				[user_settings_to_string(NewUserSettings),RejectReason] ),
			
			?wooper_return_state_result( State, user_registration_failed )

	end.	



% Unegisters specified user.
%
% Unregisters an Orge user, specified by login. The corresponding account will
% be marked as deleted.
%
% The caller is notified of the result of the operation: user_unregistered or
% {user_unregistration_failed,AtomReason} is sent back, where AtomReason may be
% login_not_known, account_already_deleted, write_transaction_failed, etc.
%
% (request)
unregisterUser( State, UserLogin ) ->

	case get_user_identifier_from_login( binarize_field(UserLogin) ) of
	
		not_found ->

			?warning_fmt( "Unregistering of user failed: unknown login '~s'",
				[UserLogin] ),

			?wooper_return_state_result( State, 
							{user_unregistration_failed,login_not_known} );
		
		UserId ->
		
			F = fun() -> 
				[CurrentUserInfos] = mnesia:read( orge_user, UserId, write ),
				case CurrentUserInfos#orge_user.account_status of
			
					deleted ->
						account_already_deleted;

					_NonDeletedAccount ->
						UpdatedUserInfos = CurrentUserInfos#orge_user{
							account_deletion_time = basic_utils:get_timestamp(),
							account_status = deleted },
						mnesia:write( UpdatedUserInfos ),
						deletion_success
						
				end	

			end,
			
			case mnesia:transaction(F) of
			
				{atomic,account_already_deleted} ->

					?warning_fmt( "Unregistering of user failed for login '~s':"
						" account already deleted", [UserLogin] ),

					?wooper_return_state_result( State, 
						 {user_unregistration_failed,account_already_deleted} );

			
				{atomic,deletion_success} ->
					?wooper_return_state_result( State, user_unregistered );
			
				{aborted,FailReason} ->

					?error_fmt(	
					   "Unregistering of user failed for login '~s': ~w", 
					   [ UserLogin, FailReason ] ),

					?wooper_return_state_result( State, 
                       {user_unregistration_failed,write_transaction_failed} )

			end
					
	end.



% Connection-related operations, mostly requests sent by client managers:


% Section for the validity checking of a user login.


% Tells whether at least one connection slot is available and, if yes, reserves
% it.
%
% To be called by client managers.
%
% (request)
requestSlot( State, ConnectionId, ClientNetId ) ->

	MaxCount = ?getAttr(maximum_slot_count),
	
	Answer = case get_active_connection_count() of 
	
		MaxCount ->
			record_slot_refused( ConnectionId, ClientNetId, State );
			
		_LessThanMaxCount ->
			record_slot_obtained( ConnectionId, ClientNetId, State )
	
	end,

	?wooper_return_state_result( State, {ConnectionId,Answer} ).



% Checks specified login request, logs it appropriately in database, and
% performs the login, if successful.
%
% Note: PasswordHash is actually an hash of the password.
%
% (request)
tryLogin( State, StringLogin, Password, ConnectionId ) ->

	Login = binarize_field(StringLogin),
	PasswordHash = get_hash(Password),

	% First, check that login is known and that password matches:
	Answer = case get_user_identifier_from_login( Login ) of
	
		not_found ->
			record_bad_login( Login, PasswordHash, ConnectionId, State );
		
		UserId ->

			% Gets the corresponding user record:
			F = fun() ->
				mnesia:read( orge_user, UserId, read )
			end,	
			
			case mnesia:transaction(F) of

				% Useless to match also with Login.
				% At most one record should match:   
				{atomic, [ #orge_user{ role=Role, 
						account_status=AccountStatus, 
						account_password=PasswordHash } ] } ->
					
					% Matching here with the already bound PasswordHash:
					case AccountStatus of 
			
						active ->
			
							% Login/password ok, account active, check this user
							% is not already connected currently:
							case get_running_connection_id_for(UserId) of
			
								not_connected ->
									% OK, connection can then proceed:
									record_access_granted( Login, PasswordHash,
										ConnectionId, UserId, Role, State );
													
								OtherConnectionId ->
									% Failure, as already connected:
									record_already_connected( Login, 
										PasswordHash, ConnectionId,
										OtherConnectionId, UserId, State )
					
							end;
			
						OtherAccountStatus ->
							record_account_not_active( Login, PasswordHash,
								ConnectionId, OtherAccountStatus, UserId, 
								State )
					
					end;
					
				{atomic, [ #orge_user{ 
						account_password = CorrectPassword } ] } ->
					% We have here a match, but with a different password:
					record_bad_password( UserId, Login, PasswordHash,
						CorrectPassword, ConnectionId, State )

			end
		   
	end,
	?wooper_return_state_result( State, {ConnectionId,Answer} ).



% Records the fact that marshalling for a connection failed.
%
% (oneway)
declareMarshallingFailed( State, ConnectionId ) ->
	record_marshalling_failed( ConnectionId, State ),
	?wooper_return_state_only( State ).



% Records the fact that a connection timed-out.
%
% (oneway)
declareTimedOut( State, ConnectionId ) ->
	record_timed_out( ConnectionId, State ),
	?wooper_return_state_only( State ).



% Records the fact that a connection with an incompatible declared version was
% attempted.
%
% (oneway)
declareIncompatible_version( State, ConnectionId, ClientVersion ) ->
	record_incompatible_version( ConnectionId, ClientVersion, State ),
	?wooper_return_state_only( State ).



% Records the fact that a connection was terminated because of a server
% shutdown.
%
% (oneway)
declareShutdownByServer( State, ConnectionId ) ->
	record_shutdown_by_server( ConnectionId, State ),
	?wooper_return_state_only( State ).


% Records the fact that a connection was terminated unexpectedly, on the client
% side.
%
% (oneway)
declareUnexpectedClientTermination( State, ConnectionId ) ->	
	record_unexpected_client_termination( ConnectionId, State ),
	?wooper_return_state_only( State ).



% Records the fact that a connection was terminated normally, by the client.
%
% (oneway)
declareNormalClientSideTermination( State, ConnectionId ) ->	
	record_normal_client_side_termination( ConnectionId, State ),
	?wooper_return_state_only( State ).




% Listing of informations.


% Returns the list of all past and current users, regardless of their state.
%
% (const request)
listAllUsers( State ) ->
	?wooper_return_state_result( State,	list_table(orge_user) ).


% Returns the list of all past and current connections, regardless of their
% state.
%
% (const request)
listAllConnections( State ) ->
	?wooper_return_state_result( State, list_table(orge_connection) ).



% Returns the list of connection identifiers corresponding to all the
% connections fully accepted and running.
%
% (const request)
listAllActiveConnectionIdentifiers( State ) ->
	?wooper_return_state_result( State,	list_active_connection_identifiers() ).
					

	
% Operations on the database itself:


% Returns a textual description of the state of this database.
%
% (const request)
getStatus( State ) ->

	% FIXME: retrieve more infos from mnesia:system_info.

	Infos = io_lib:format( "database running, the identifier that will "
						   "be assigned to the next user is ~B, "
						   "maximum slot count is ~B", 
		[ ?getAttr(current_user_id), ?getAttr(maximum_slot_count) ] ),

	?wooper_return_state_result( State, Infos ).
		


% Triggers a backup of the Orge database.
%
% (oneway).
backup( _State ) ->
	% FIXME:
	throw( backup_not_implemented ).


				

% Deletes all entries in all the Orge tables.
%
% (oneway)
resetTables( State ) ->
	
	?info( "Resetting tables of Orge database." ),

	lists:foreach( 
		fun(Table) -> 
			{atomic,ok} = mnesia:clear_table(Table)
		end,
		?table_list ),
	?wooper_return_state_only( State ).



% Stops the Orge database.
%
% (request)
stop( State ) ->

	?info( "Stopping Orge database." ),
    mnesia:stop(),
	?trace( "Orge database stopped." ),
	?wooper_return_state_result( State, orge_database_stopped ).






% Section for user-related operations.


% User query section.
	
	
% Returns the identifiers of all past and current users, regardless of their
% state.
list_all_user_identifiers() ->
	F = fun() ->
		MatchHead = #orge_user{ identifier='$1', _='_'},
		Result = '$1',
		mnesia:select( orge_user, [ {MatchHead, _Guard=[], [Result]} ] )
	end,
	{atomic,UserIdList} = mnesia:transaction(F),
	UserIdList.


% Determines from the user table what is the highest user identifier used.
%
% Note: useful when restoring the state of a database. 
%
% Maybe there is a more efficient way of determining the highest primary key, as
% an index over integer keys probably sorts them.
get_highest_user_number() ->
	
	% Works even if no user was recorded, as starting at 0:
	case list_all_user_identifiers() of
	
		[] ->
			0;
			
		NonEmptyList ->
			lists:max( NonEmptyList )

	end.

	
	
	
% Connection query section.

		
% Returns the list of the identifiers of all known connections.
list_all_connection_identifiers() ->
	F = fun() ->
		MatchHead = #orge_connection{ identifier='$1', _='_'},
		Result = '$1',
		mnesia:select( orge_connection, [ {MatchHead, _Guard=[], [Result]} ] )
	end,
	{atomic,ConnectionIdList} = mnesia:transaction(F),
	ConnectionIdList.
	
	
	
% Determines from the connection table what is the highest connection identifier
% used.
%
% Note: useful when restoring the state of a database.
%
% Maybe there is a more efficient way of determining the highest primary key, as
% an index over integer keys probably sorts them.
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
		MatchHead = #orge_connection{ identifier='$1', 
			status=access_granted, _='_'},
		Result = '$1',
		mnesia:select( orge_connection, [ {MatchHead, _Guard=[], [Result]} ] )
	end,
	{atomic,ActiveConnectionIdList} = mnesia:transaction(F),
	ActiveConnectionIdList.	
	%manage_query( qlc:q([ X#orge_connection.identifier 
	%	|| X <- mnesia:table(orge_connection),
	%	X#orge_connection.status =:= access_granted ]) ).
	
	
	
% Returns the current number of active connections (used slots).
get_active_connection_count() ->	
	length( list_active_connection_identifiers() ).



		
% Database-generic operation section.		
		
	   
% Lists all entries of the specified table.
%
% Returns the entry list.
list_table(TableName) ->
	mnesia:transaction( 

	  fun() ->
		qlc:eval( qlc:q( [ X || X <- mnesia:table(TableName) ] ) )
	  end 
	 
	).
	%{atomic,TableRecords} = mnesia:select( _MatchHead = ..., etc.
	%TableRecords.
	
	

% Includes only this node for the moment:
get_database_nodes() ->
	[ net_utils:localnode() ].
	
	


% Database prebuilt convenience queries.


% Returns either not_found, or the identifier of the user record corresponding
% to the specified login.
get_user_identifier_from_login( AccountLogin ) ->
	
	F = fun() ->
		MatchHead = #orge_user{ identifier='$1', 
			account_login=AccountLogin, _='_' },
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



% Returns either not_connected, or the orge_connection record corresponding to
% specified Orge user identifier, supposed to be already connected.
get_running_connection_id_for( UserId ) ->

	F = fun() ->
		MatchHead = #orge_connection{ identifier='$1', 
			status=access_granted, user_identifier = UserId, _='_' },
		Result = '$1',
		mnesia:select( orge_connection, [ {MatchHead, _Guard=[], [Result]} ] )
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
			
	end.
	


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

create_tables_on( DatabaseNodes, State ) ->

	?trace_fmt( "Creating tables on following node(s): ~w.", [DatabaseNodes] ),
	
	?debug( "Creating table for Orge users." ),
	{atomic,ok} = mnesia:create_table( orge_user, [ 
		{attributes, record_info(fields,orge_user)},
		{disc_copies,DatabaseNodes},
		{type,set} ] ),
		
	?debug( "Creating table for Orge connections." ),
	{atomic,ok} = mnesia:create_table( orge_connection, [ 
		{attributes, record_info(fields,orge_connection)},
		{disc_copies,DatabaseNodes},
		{type,set} ] ).		
	


% Returns the orge_user record OrgeUser, whose fields have been updated from the
% OrgeUserSettings orge_user_settings record.
update_from_settings( OrgeUser, OrgeUserSettings ) ->
	?user{
		account_login     = ?user_settings.account_login,
		account_password  = ?user_settings.account_password,
		first_name        = ?user_settings.first_name,
		last_name         = ?user_settings.last_name,
		date_of_birth     = ?user_settings.date_of_birth,
		address_line_1    = ?user_settings.address_line_1,
		address_line_2    = ?user_settings.address_line_2,
		city              = ?user_settings.city,
		state             = ?user_settings.state,
		country           = ?user_settings.country,
		postal_code       = ?user_settings.postal_code,
		home_phone        = ?user_settings.home_phone,
		mobile_phone      = ?user_settings.mobile_phone,
		email_address     = ?user_settings.email_address,
		security_question =	?user_settings.security_question,
		security_answer   = ?user_settings.security_answer
	}.
	
	
	
% Returns an updated user settings record where fields which are strings are
% binaries instead of list of characters.
binarize_user( OrgeUser ) ->
	?user{
		account_login     = binarize_field( ?user.account_login ),
		account_password  = binarize_field( ?user.account_password ),
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
		security_question = binarize_field( ?user.security_question ),
		security_answer   = binarize_field( ?user.security_answer )
	}.
		


% Returns a translation of specified term targeting the storage in database.
binarize_field( Field ) ->
	
	case text_utils:is_string(Field) of
	
		true -> 
			% The list must be a string:
			% Currently deactivated, as displayed as '#Bin' in tv application:
			%text_utils:string_to_binary( Field );
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
		" account login is '~s', hash of account password is '~s', "
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
	
	io_lib:format( "Account #~s created on ~s and ~s, with role ~w and ~s. "
		"Current account status: ~s. "
		"User first name is '~s', last name is '~s', date of birth is '~s', "
		"first address line is '~s', second one is '~s', city is '~s', "
		"state is '~s', country is '~s', postal code is '~s', "
		"home telephone is '~s', mobile one is '~s', e-mail address is '~s', "
		" account login is '~s', hash of account password is '~s', "
		"security question is '~s', security answer is '~s'",
		[ 
			text_utils:integer_to_string( ?user.identifier ),
			basic_utils:timestamp_to_string( 
				?user.account_creation_time ),
			DeleteString,
			?user.role,
			CharacterString,
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
%
% Note: no direct pattern-matching on status to avoid to have to bind
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
		
		shutdown_by_server ->
			connection_shutdown_by_server_to_string(OrgeConnection);
	
		unexpected_client_termination ->
			connection_unexpected_client_termination_to_string(OrgeConnection);
		
		normal_client_side_termination ->
			connection_normal_client_side_termination_to_string(OrgeConnection)
					
	end,
	String ++ " " ++ geolocation_to_string(OrgeConnection).
	
	




% Section about textual descriptions of connections.	



connection_slot_refused_to_string(OrgeConnection) ->

	io_lib:format( "Connection #~s refused by the server "
		"due to a lack of available slot at ~s from host ~s whose ~s.",
		[ 
			text_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.stop_time ),
			net_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port	),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns )
		] ).
		
		
		
connection_slot_obtained_to_string(OrgeConnection) ->
	io_lib:format( "Connection #~s obtained a server slot "
		"at ~s from host ~s whose ~s.",
		[ 
			text_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time ),
			net_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port	),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns )
		] ).



connection_bad_login_to_string(OrgeConnection) ->
	io_lib:format( "Failed connection #~s due to bad login '~s' "
		"(with specified password hash '~s') from host ~s whose ~s at ~s.",
		[ 
			text_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.password,		
			net_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.stop_time )
		] ).



connection_bad_password_to_string(OrgeConnection) ->
	io_lib:format( "Failed connection #~s due to bad password hash '~s' "
		"for known login '~s' (user #~B) from host ~s whose ~s at ~s.",
		[ 
			text_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			OrgeConnection#orge_connection.password,		
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.user_identifier,
			net_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.stop_time )
		] ).



connection_account_not_active_to_string(OrgeConnection) ->
	io_lib:format( "Failed connection #~s due to a non-active account "
		"for Orge user #~s (login: '~s', password hash: '~s'), "
		"from host ~s whose ~s, at ~s.",
		[ 
			text_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			text_utils:integer_to_string(
				OrgeConnection#orge_connection.user_identifier ),
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.password,		
			net_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.stop_time )
		] ).
		
		
		
connection_already_connected_to_string(OrgeConnection) ->
	io_lib:format( "Failed connection #~s due to an already existing connection"
		" for Orge user #~s (login: '~s', password hash: '~s'), "
		"from host ~s whose ~s, at ~s.",
		[ 
			text_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			text_utils:integer_to_string(
				OrgeConnection#orge_connection.user_identifier ),
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.password,		
			net_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.stop_time )
		] ).



connection_marshalling_failed_to_string(OrgeConnection) ->
	io_lib:format( "Failed marshalling for connection #~s at ~s "
		"from host ~s whose ~s.",
		[ 
			text_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.stop_time ),
			net_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port	),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns )
		] ).



connection_timed_out_to_string(OrgeConnection) ->
	io_lib:format( "Time-out for connection #~s at ~s from host ~s whose ~s.",
		[ 
			text_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.stop_time ),
			net_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port	),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns )
		] ).



connection_access_granted_to_string(OrgeConnection) ->
	io_lib:format( "Active connection #~s of Orge user #~s "
		"(login: '~s', password hash: '~s') from host ~s whose ~s, "
		"started at ~s.",
		[ 
			text_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			text_utils:integer_to_string(
				OrgeConnection#orge_connection.user_identifier ),
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.password,
			net_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.start_time )
		] ).
						


connection_incompatible_version_to_string(OrgeConnection) ->
	io_lib:format( "Connection #~s terminated due to "
		"an incompatible client version (~s) "
		"for Orge user #~s (login: '~s', password hash: '~s'), "
		"from host ~s whose ~s, at ~s.",
		[ 
			text_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			text_utils:version_to_string( 
				OrgeConnection#orge_connection.client_version ),
			text_utils:integer_to_string(
				OrgeConnection#orge_connection.user_identifier ),
			OrgeConnection#orge_connection.login,
			OrgeConnection#orge_connection.password,		
			net_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.stop_time )
		] ).



connection_shutdown_by_server_to_string(OrgeConnection) ->
	io_lib:format( "Connection #~s of user #~s terminated due to "
		"server shutdown, from host ~s whose ~s, at ~s.",
		[ 
			text_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			text_utils:integer_to_string(
				OrgeConnection#orge_connection.user_identifier ),
			net_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.stop_time )
		] ).



connection_unexpected_client_termination_to_string(OrgeConnection) ->
	io_lib:format( "Connection #~s of user #~s terminated unexpectedly "
		"from client-side, from host ~s whose ~s, at ~s.",
		[ 
			text_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			text_utils:integer_to_string(
				OrgeConnection#orge_connection.user_identifier ),
			net_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.stop_time )
		] ).
		
		
		
connection_normal_client_side_termination_to_string(OrgeConnection) ->
	io_lib:format( "Connection #~s of user #~s terminated normally  "
		"from client-side, from host ~s whose ~s, at ~s.",
		[ 
			text_utils:integer_to_string( 
				OrgeConnection#orge_connection.identifier ),
			text_utils:integer_to_string(
				OrgeConnection#orge_connection.user_identifier ),
			net_utils:ipv4_to_string( 
				OrgeConnection#orge_connection.ip_address,
				OrgeConnection#orge_connection.port
			),
			reverse_dns_to_string( OrgeConnection#orge_connection.reverse_dns ),
			basic_utils:timestamp_to_string( 
				OrgeConnection#orge_connection.stop_time )
		] ).




% Returns a textual description of the login status.
	
status_to_string(slot_refused) ->
	"failed connection, short of having one available server slot";
	
status_to_string(slot_obtained) ->
	"connection which obtained a server slot";
	
status_to_string(bad_login) ->
	"failed login due to bad login";
	
status_to_string(bad_password) ->
	"failed login due to bad password";
	
status_to_string(account_not_active) ->
	"failed login since user account not active";

status_to_string(already_connected) ->
	"failed login since user already logged-in";

status_to_string(marshalling_failed) ->
	"failed login due to client ill-formatted identifier data";

status_to_string(timed_out) ->
	"failed login due to client time-out";
	
status_to_string(access_granted) ->
	"successfully logged-in";
	
status_to_string(incompatible_version) ->
	"failed connection due to incompatible client version";
	
status_to_string(shutdown_by_server) ->
	"stopped connection due to server shutdown";
	
status_to_string(unexpected_client_termination) ->
	"normally terminated connection".
	


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
	




% Section for the validity checking of user information (on initial
% registration).



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
	
	
		
				



% Section to record state changes in the connection.  
%
% Note: the two slot record functions are the places where the connection entry
% is created.


% Creates a connection entry for that initial slot request, which was refused.
record_slot_refused( ConnectionId, {ClientIP,ClientPort,ReversedDNS},
				   State ) ->

	% Creating the new connection entry with first information:
	NewConnection = #orge_connection{
		identifier = ConnectionId,
		status = slot_refused,
		ip_address = ClientIP,
		port = ClientPort,
		reverse_dns = ReversedDNS,
		start_time = basic_utils:get_timestamp(),
		stop_time  = basic_utils:get_timestamp()
	},
	
	LocatedConnection = add_geolocation_infos( NewConnection, State ),
	
	F = fun() -> mnesia:write( LocatedConnection ) end,

	case mnesia:transaction(F) of
	
		{atomic,_} ->
			?info_fmt( "Slot refused for connection #~B.", [ConnectionId] ),
			slot_refused;
							
		{aborted,FailReason} ->
			?error_fmt(
				"Registering of slot rejection for connection (~s) failed: ~w",
					[connection_to_string(NewConnection),FailReason] ),
			internal_error	
	
	end.	
	
	
	
% Creates a connection entry for that initial slot request, which was accepted.
%
% Note: this is where the connection entry is created.
record_slot_obtained( ConnectionId, {ClientIP,ClientPort,ReversedDNS},
					State ) ->

	% Creating the new connection entry with first information:
	NewConnection = #orge_connection{
		identifier = ConnectionId,
		status = slot_obtained,
		ip_address = ClientIP,
		port = ClientPort,
		reverse_dns = ReversedDNS,
		start_time = basic_utils:get_timestamp()
	},
	
	LocatedConnection = add_geolocation_infos( NewConnection, State ),
	
	F = fun() -> mnesia:write( LocatedConnection ) end,
	
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			?info_fmt( "Slot obtained for connection #~B.",	[ConnectionId] ),
			slot_obtained;
							
		{aborted,FailReason} ->
			?error_fmt(
				"Registering of slot obtainment for connection (~s) failed: ~w",
				[connection_to_string(NewConnection),FailReason] ),
			internal_error	
	
	end.	



% Records that connection and returns an atom describing its status.
record_bad_login( Login, Password, ConnectionId, State ) ->

	% Just updating the corresponding connection entry:		
	F = fun() -> 
			[CurrentConnection] = mnesia:read( orge_connection, ConnectionId,
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
			?warning_fmt( "Bad login ('~s') specified, "
				"with password hash '~s' (connection #~B).", 
				[ Login, Password, ConnectionId ] ),
			bad_login;
							
		{aborted,FailReason} ->
			?error_fmt( 
			   "Registering of connection #~B with bad login failed: ~w",
			   [ConnectionId,FailReason] ),
			internal_error	
	
	end.	
	
	
	
% Records that connection and returns an atom describing its status.
record_bad_password( UserId, Login, WrongPassword, CorrectPassword, 
		ConnectionId, State ) ->

	% Just updating the corresponding connection entry:		
	F = fun() -> 
			[CurrentConnection] = mnesia:read( orge_connection, ConnectionId,
				write ),
			UpdatedConnection = CurrentConnection#orge_connection{
				status = bad_password,
				user_identifier = UserId,
				login = Login,
				password = WrongPassword,
				stop_time = basic_utils:get_timestamp()
			},
			mnesia:write( UpdatedConnection ) 
		
		end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			?warning_fmt( "Bad password hash ('~s') specified for login "
				"'~s' instead of password hash '~s' (connection #~B).",
				[ WrongPassword, CorrectPassword, Login, ConnectionId ] ),
			bad_password;
							
		{aborted,FailReason} ->
			?error_fmt(
				"Registering of connection #~B with bad password failed: ~w",
					[ConnectionId,FailReason] ),
			internal_error	
	
	end.	



% Records that connection and returns an atom describing its status.
record_account_not_active( Login, Password,	ConnectionId, AccountStatus,
		UserId, State ) ->

	% Just updating the corresponding connection entry:		
	F = fun() -> 
			[CurrentConnection] = mnesia:read( orge_connection, ConnectionId,
				write ),
			UpdatedConnection = CurrentConnection#orge_connection{
				status = account_not_active,
				user_identifier = UserId,
				login = Login,
				password = Password,
				stop_time = basic_utils:get_timestamp()
			},
			mnesia:write( UpdatedConnection ) 
		
		end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			?warning_fmt( "Connection #~B with login ~p disallowed, "
				"as the corresponding account of user #~B "
				"is not active (status: ~s).", 
				[ ConnectionId, Login, UserId, AccountStatus ] ),
			account_not_active;
							
		{aborted,FailReason} ->
			?error_fmt( "Registering of connection #~B to a non-active account "
				"failed: ~w", [ConnectionId,FailReason] ),
			internal_error	
	
	end.	
	

	
% Records that connection and returns an atom describing its status.
record_already_connected( Login, Password, ConnectionId, OtherConnectionId,
		UserId, State ) ->
			
	% Just updating the corresponding connection entry:		
	F = fun() -> 
			[CurrentConnection] = mnesia:read( orge_connection, ConnectionId,
				write ),
			UpdatedConnection = CurrentConnection#orge_connection{
				status = already_connected,
				user_identifier = UserId,
				login = Login,
				password = Password,
				stop_time = basic_utils:get_timestamp()
			},
			mnesia:write( UpdatedConnection ) 
		
		end,
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			?warning_fmt( "Connection #~B from user ~B ('~s') disallowed, "
				"as is already connected as connection #~B.", 
				[ ConnectionId, UserId, Login, OtherConnectionId ] ),
			already_connected;
							
		{aborted,FailReason} ->
			?error_fmt( 
			   "Registering of already existing connection #~B failed: ~w",
			   [ConnectionId,FailReason] ),
			internal_error	
	
	end.	
	


% Records that connection and returns an atom describing its status.
% Garbled identifiers could be stored maybe.
record_marshalling_failed( ConnectionId, State ) ->

	% Just updating the corresponding connection entry:		
	F = fun() -> 
			[CurrentConnection] = mnesia:read( orge_connection, ConnectionId,
				write ),
			UpdatedConnection = CurrentConnection#orge_connection{
				status = marshalling_failed,
				stop_time = basic_utils:get_timestamp()
			},
			mnesia:write( UpdatedConnection ) 
		
		end,
	
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			?warning_fmt( "Marshalling failed for connection #~B.", 
						  [ ConnectionId ] ),
			marshalling_failed;
							
		{aborted,FailReason} ->
			?error_fmt( "Registering of connection #~B "
				"with marshalling failure failed: ~w",
				[ConnectionId,FailReason] ),
			internal_error	
	
	end.	


	
% Records that connection and returns an atom describing its status.
record_timed_out( ConnectionId, State ) ->
	
	% Just updating the corresponding connection entry:		
	F = fun() -> 
			[CurrentConnection] = mnesia:read( orge_connection, ConnectionId,
				write ),
			UpdatedConnection = CurrentConnection#orge_connection{
				status = timed_out,
				stop_time = basic_utils:get_timestamp()
			},
			mnesia:write( UpdatedConnection ) 
		
		end,
	
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			?warning_fmt( "Connection time-out for connection #~B.", 
						  [ ConnectionId ] ),
			timed_out;
							
		{aborted,FailReason} ->
			?error_fmt( 
			   "Registering of connection #~B with time-out failed: ~w",
			   [ConnectionId,FailReason] ),
			internal_error	
	
	end.	



% Records that connection and returns an atom describing its status.
record_access_granted( Login, Password, ConnectionId, UserId, Role, State ) ->

	% Just updating the corresponding connection entry:		
	F = fun() -> 
			[CurrentConnection] = mnesia:read( orge_connection, ConnectionId,
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
			?info_fmt( "Access granted for user #~B "
				"(login: '~s', password hash: '~s', role: ~w) "
				"for connection #~B.",
				[ UserId, Login, Password, Role, ConnectionId ] ),
			access_granted;
							
		{aborted,FailReason} ->
			?error_fmt(	"Registering of granted connection #~B failed: ~w",
					[ConnectionId,FailReason] ),
			internal_error	
	
	end.	



% Records that connection and returns an atom describing its status.
record_incompatible_version( ConnectionId, ClientVersion, State ) ->

	% Just updating the corresponding connection entry:		
	F = fun() -> 
			[CurrentConnection] = mnesia:read( orge_connection, ConnectionId,
				write ),
			UpdatedConnection = CurrentConnection#orge_connection{
				status = incompatible_version,
				client_version = ClientVersion,
				stop_time = basic_utils:get_timestamp()
			},
			mnesia:write( UpdatedConnection ) 
		
		end,
	
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			?debug_fmt(
				"Connection #~B failed due to incompatible client version ~s.",
				[ ConnectionId, text_utils:version_to_string(ClientVersion) ] ),
			ok;
							
		{aborted,FailReason} ->
			?error_fmt(
				"Registering of client version for connection #~B failed: ~w",
				[ConnectionId,FailReason] ),
			internal_error	
	
	end.	



% Records that connection and returns an atom describing its status.
record_shutdown_by_server( ConnectionId, State ) ->

	% Just updating the corresponding connection entry:		
	F = fun() -> 
			[CurrentConnection] = mnesia:read( orge_connection, ConnectionId,
				write ),
			UpdatedConnection = CurrentConnection#orge_connection{
				status = shutdown_by_server,
				stop_time = basic_utils:get_timestamp()
			},
			mnesia:write( UpdatedConnection ) 
		
		end,
	
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			?debug_fmt( "Connection  #~B stopped due to server shutdown.", 
						[ ConnectionId ] ),
			ok;
							
		{aborted,FailReason} ->
			?error_fmt(
				"Registering of client version for connection #~B failed: ~w",
				[ConnectionId,FailReason] ),
			internal_error	
	
	end.	


	
% Updates that connection and returns an atom describing its status.
record_unexpected_client_termination( ConnectionId, State ) ->

	% Just updating the corresponding connection entry:		
	F = fun() -> 
			[CurrentConnection] = mnesia:read( orge_connection, ConnectionId,
				write ),
			UpdatedConnection = CurrentConnection#orge_connection{
				status = unexpected_client_termination,
				stop_time = basic_utils:get_timestamp()
			},
			mnesia:write( UpdatedConnection ) 
		
		end,
	
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			?trace_fmt(	"Connection #~B terminated by client unexpectedly.", 
				[ ConnectionId ] ),
			unexpected_client_termination;
							
		{aborted,FailReason} ->
			?error_fmt( "Updating of connection #~B "
				"after an unexpected client-side termination failed: ~w",
				[ConnectionId,FailReason] ),
			internal_error	
	
	end.	
	


% Updates that connection and returns an atom describing its status.
record_normal_client_side_termination( ConnectionId, State ) ->

	% Just updating the corresponding connection entry:		
	F = fun() -> 
			[CurrentConnection] = mnesia:read( orge_connection, ConnectionId,
				write ),
			UpdatedConnection = CurrentConnection#orge_connection{
				status = normal_client_side_termination,
				stop_time = basic_utils:get_timestamp()
			},
			mnesia:write( UpdatedConnection ) 
		
		end,
	
	case mnesia:transaction(F) of
	
		{atomic,_} ->
			?trace_fmt(	"Connection #~B terminated by client normally.", 
				[ ConnectionId ] ),
			normal_client_side_termination;
							
		{aborted,FailReason} ->
			?error_fmt( "Updating of connection #~B "
				"after a normal client-side termination failed: ~w",
				[ConnectionId,FailReason] ),
			internal_error	
	
	end.	
	



% Updates connection records with IP geolocation informations.
add_geolocation_infos( Connection, State ) ->

	TargetAddress = net_utils:ipv4_to_string( 
		Connection#orge_connection.ip_address ),
		
	case egeoip:lookup( TargetAddress ) of
	
		{ ok, {geoip,_CountryCode, _OtherCountryCode, CountryName, Region, City,
				PostalCode, _Latitude, _Longitude, _AreaCode, _DMACode } } ->
			Connection#orge_connection{
				geolocated_country = CountryName,
				% Unless binarize_field is a no-op, will perform a useless
				% double conversion:
				geolocated_region = binarize_field( binary_to_list(Region) ),
				geolocated_city   = binarize_field( binary_to_list(City) ),
				geolocated_postal_code = binarize_field(
					binary_to_list(PostalCode) )
			};
			
		{ error, Reason } ->
			?error_fmt( "Geolocation of ~s failed: ~w",	[TargetAddress,Reason]),
			% Returning as is:	
			Connection
			
	end.
	
	
	
% Returns the hash code, as a binary, corresponding to the specified password.
get_hash(Password) ->
	%crypto:md5(Password).
	crypto:sha(Password).		


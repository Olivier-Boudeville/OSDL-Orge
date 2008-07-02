% Orge database manager.
%
-module(orge_database_manager).

-export([start/1,start_link/1,
	orge_user_settings_to_string/1,orge_user_to_string/1]).



% Implementation notes.

% This database manager uses Mnesia to store the various informations 
% in specified in OrgeDatabase.rst.
%
% To each table corresponds a dedicated record, and the two will be named
% identically (useful with mnesia:write/1, for example).
% The first attribute of the record is the primary key.
%
% Access to the database will be transaction-based.
% The user table (orge_user) will be created in RAM and on disk as well.


% Necessary to call QLC query language functions:
-include_lib("stdlib/include/qlc.hrl").

% For orge_user_settings and all:
-include("orge_database_manager.hrl").

% For emit_*:
-include("traces.hrl").


% Shortcut macro, for convenience: 
-define(getState,DatabaseState#database_state).



% Describes the Mnesia internal entry type (fields) for Orge users:
% (correspond to a non-nested orge_user_settings record, plus other fields)
-record( orge_user, 
	{
	
		% Internal fields:
		identifier,
		account_creation_time,
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


% List of all the tables of interest in the Orge database:
-define(table_list,[orge_user]).



% Starts an Orge database and returns its PID.
% InitMode can be:
%   - from_scratch: to start a fresh new (blank) database
%   - from_previous_state: to start a previously existing database
start(InitMode) ->
	spawn( fun() -> init(InitMode) end ).


% Starts a linked Orge database and returns its PID.
% InitMode can be:
%   - from_scratch: to start a fresh new (blank) database
%   - from_previous_state: to start a previously existing database
start_link(InitMode) ->
	spawn_link( fun() -> init(InitMode) end ).


	

	
% Helper functions.


% init returns the first database state.
init(from_scratch) ->
	?emit_info([ "Starting Orge database, created from scratch." ]),
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
	?emit_trace( [ "Creating schema." ] ),
    ok = mnesia:create_schema( DatabaseNodes ),
    ok = mnesia:start(),
	create_tables( DatabaseNodes ),
	loop( #database_state{ current_user_id=1 } );
	
init(from_previous_state) ->	
	?emit_info([ "Starting Orge database from previous state." ]),
	ok = mnesia:start(),
	% Hangs until all tables in the list are accessible, or until the 10s
	% time-out occurs:
    ok = mnesia:wait_for_tables(?table_list, 4000),
	loop( #database_state{ current_user_id=1 } ).


% The database main loop.
loop( DatabaseState ) ->
	?emit_debug([ "Database waiting for requests." ]),
	receive
	
		{register_user,UserSettings,CallerPid} ->
			loop( register_user( DatabaseState,UserSettings,CallerPid ) );
		
		{list_users,CallerPid} ->
			CallerPid ! {orge_users,list_table(orge_user)},
			loop( DatabaseState );
			
		reset ->
			loop( reset_tables(DatabaseState) );
				
		{stop,CallerPid} ->
			stop(DatabaseState,CallerPid);
		
		Other ->
			?emit_warning([ io_lib:format( 
				"Database ignored following message: ~s.",
				[orge_user_settings_to_string(Other)] ) ])
	
	end.


% Registers specified Orge user settings, where NewUserSettings is an 
% orge_user_settings, in one transaction.
% The caller is notified of the result of the operation register_success or
% {register_failed,AtomReason} is sent back, where AtomReason may be
% login_already_registered, write_transaction_failed, etc.
% Returns an updated state.
register_user(DatabaseState,NewUserSettings,CallerPid) ->
	case check_validity(NewUserSettings,DatabaseState) of 
	
		ok -> 
			ThisUserId = DatabaseState#database_state.current_user_id, 
			NewUser = update_from_settings( #orge_user{  
				identifier = ThisUserId,
				account_creation_time = utils:get_timestamp(),
				characters = none
			}, NewUserSettings),	
			F = fun() -> mnesia:write( NewUser ) end,
			case mnesia:transaction(F) of
			
				{atomic,_} ->
					CallerPid ! register_success,
					DatabaseState#database_state{ 
						current_user_id = ThisUserId + 1 };
				
				{aborted,FailReason} ->
					?emit_error([ io_lib:format(
						"Registering of user settings (~s) failed: ~w",
						[orge_user_settings_to_string(NewUserSettings),
							FailReason] ) ]),
					CallerPid ! {register_failed,write_transaction_failed},
					DatabaseState
			end;		

		RejectReason ->
			?emit_warning([ io_lib:format(
				"Registering of user settings (~s) failed: ~w",
				[orge_user_settings_to_string(NewUserSettings),
					RejectReason] ) ]),
			CallerPid ! {register_failed,RejectReason},
			DatabaseState
		
	end.	
	

% Lists all entries of specified table.
% Returns the entry list.
list_table(TableName) ->
	manage_query( qlc:q([ X || X <- mnesia:table(TableName) ]) ).


% Deletes all entries in the Orge tables.
reset_tables(DatabaseState) ->
	?emit_info([ "Resetting tables of Orge database." ]),
	lists:foreach( fun(Table) -> {atomic, ok} = mnesia:clear_table(Table) end,
		?table_list ),
	DatabaseState.


% Stops the database.
stop(_DatabaseState,CallerPid) ->
	?emit_info([ "Stopping Orge database." ]),
    mnesia:stop(),
	CallerPid ! database_stopped.


% Includes only this node for the moment:
get_database_nodes() ->
	[node()].
	
	
get_user_from_login( AccountLogin ) ->
	case manage_query( qlc:q([ X || X <- mnesia:table(orge_user),
			X#orge_user.account_login =:= AccountLogin ]) ) of 
		
		[] ->
			not_found;
			
		% At most one entry should match:	
		[UserInfo] ->
			UserInfo	
			
	end	.


% record_info cannot take variable arguments.
%create_tables(TableList,DatabaseNodes) ->
%	lists:foreach( fun(TableName) ->
%		{atomic, ok} = mnesia:create_table( TableName, [ 
%				{attributes, record_info(fields,TableName)},
%				{disc_copies,DatabaseNodes},
%				{type,set}
%			] )	end,
%	TableList).		

create_tables(DatabaseNodes) ->
	?emit_trace( [ "Creating tables." ] ),
	{atomic, ok} = mnesia:create_table( orge_user, [ 
		{attributes, record_info(fields,orge_user)},
		{disc_copies,DatabaseNodes},
		{type,set} ] ).		
	

% Returns the orge_user record OrgeUser, whose fields have been updated 
% from the OrgeUserSettings orge_user_settings record.
update_from_settings( OrgeUser, OrgeUserSettings ) ->
	OrgeUser#orge_user{
		first_name        = OrgeUserSettings#orge_user_settings.first_name,
		last_name         = OrgeUserSettings#orge_user_settings.last_name,
		date_of_birth     = OrgeUserSettings#orge_user_settings.date_of_birth,
		address_line_1    = OrgeUserSettings#orge_user_settings.address_line_1,
		address_line_2    = OrgeUserSettings#orge_user_settings.address_line_2,
		city              = OrgeUserSettings#orge_user_settings.city,
		state             = OrgeUserSettings#orge_user_settings.state,
		country           = OrgeUserSettings#orge_user_settings.country,
		postal_code       = OrgeUserSettings#orge_user_settings.postal_code,
		home_telephone    = OrgeUserSettings#orge_user_settings.home_telephone,
		mobile_telephone  =
			OrgeUserSettings#orge_user_settings.mobile_telephone,
		email_address     = OrgeUserSettings#orge_user_settings.email_address,
		account_login     = OrgeUserSettings#orge_user_settings.account_login,
		account_password  = 
			OrgeUserSettings#orge_user_settings.account_password,
		security_question =
			OrgeUserSettings#orge_user_settings.security_question,
		security_answer   = OrgeUserSettings#orge_user_settings.security_answer
	}.
	
	
% Manages a QLC query.	
manage_query(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
 	

% Returns a textual description of specified Orge user settings record.
orge_user_settings_to_string(OrgeUserSettings) ->
	io_lib:format( 
		"User first name is '~s', last name is '~s', date of birth is '~s', "
		"first address line is '~s', second one is '~s', city is '~s', "
		"state is '~s', country is '~s', postal code is '~s', "
		"home telephone is '~s', mobile one is '~s', e-mail address is '~s', "
		" account login is '~s', account password is '~s', "
		"security question is '~s', security answer is '~s'",
		[ 
			OrgeUserSettings#orge_user_settings.first_name,
			OrgeUserSettings#orge_user_settings.last_name,
			OrgeUserSettings#orge_user_settings.date_of_birth,
			OrgeUserSettings#orge_user_settings.address_line_1,
			OrgeUserSettings#orge_user_settings.address_line_2,
			OrgeUserSettings#orge_user_settings.city,
			OrgeUserSettings#orge_user_settings.state,
			OrgeUserSettings#orge_user_settings.country,
			OrgeUserSettings#orge_user_settings.postal_code,
			OrgeUserSettings#orge_user_settings.home_telephone,
			OrgeUserSettings#orge_user_settings.mobile_telephone,
			OrgeUserSettings#orge_user_settings.email_address,
			OrgeUserSettings#orge_user_settings.account_login,
			OrgeUserSettings#orge_user_settings.account_password,
			OrgeUserSettings#orge_user_settings.security_question,
			OrgeUserSettings#orge_user_settings.security_answer
			
		] ).


% Returns a textual description of specified Orge user record.
orge_user_to_string(OrgeUser) ->
	io_lib:format( "Account #~s created on ~s with registered characters ~w. "
		"User first name is '~s', last name is '~s', date of birth is '~s', "
		"first address line is '~s', second one is '~s', city is '~s', "
		"state is '~s', country is '~s', postal code is '~s', "
		"home telephone is '~s', mobile one is '~s', e-mail address is '~s', "
		" account login is '~s', account password is '~s', "
		"security question is '~s', security answer is '~s'",
		[ 
			utils:integer_to_string( OrgeUser#orge_user.identifier ),
			utils:timestamp_to_string( 
				OrgeUser#orge_user.account_creation_time ),
			OrgeUser#orge_user.characters,
			OrgeUser#orge_user.first_name,
			OrgeUser#orge_user.last_name,
			OrgeUser#orge_user.date_of_birth,
			OrgeUser#orge_user.address_line_1,
			OrgeUser#orge_user.address_line_2,
			OrgeUser#orge_user.city,
			OrgeUser#orge_user.state,
			OrgeUser#orge_user.country,
			OrgeUser#orge_user.postal_code,
			OrgeUser#orge_user.home_telephone,
			OrgeUser#orge_user.mobile_telephone,
			OrgeUser#orge_user.email_address,
			OrgeUser#orge_user.account_login,
			OrgeUser#orge_user.account_password,
			OrgeUser#orge_user.security_question,
			OrgeUser#orge_user.security_answer
			
		] ).





% Section for user informations validity checking.


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
	
		
check_validity_of_account( AccountLogin, _AccountPassword ) ->
	case get_user_from_login( AccountLogin ) of 
	
		not_found ->
			ok;
			
		_Other ->
			login_already_registered	
	
	end.
	
	
check_validity_of_security_challenge( _SecurityQuestion, _SecurityAnswer ) ->
	% TO-DO
	ok.
	

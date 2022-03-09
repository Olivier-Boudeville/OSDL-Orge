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


% Unit tests for the Orge tcp_server.
% See the orge_tcp_server and orge_tcp_client_test modules.
-module(orge_tcp_server_test).



-define(test_server_name,my_orge_tcp_test_server).


-define(Tested_modules, [orge_tcp_server] ).


% For orge_user_settings:
-include("class_DatabaseManager.hrl").


% For trace facilities:
-include("traces_for_tests.hrl").


	
run() ->

	?test_start,
	
	?test_info( "Creating a new Orge tcp_server." ),
	
	DatabaseManagement = from_scratch,
	%DatabaseManagement = from_previous_state,
	
	ServerPid = orge_tcp_server:create_link( ?test_server_name,
		DatabaseManagement, local_only ),
		
	?test_info( "Requesting server informations." ),
	ServerPid ! {self(),get_info},	
	receive
	
		{server_info,StateString} ->
			?test_info_fmt( "Current server state is: ~s.",	[StateString] )
			
	end,
	
	% See also: orge_database_manager_test.erl
	
	AlfSettings = #orge_user_settings{
		first_name = "Gordon",
		last_name = "Shumway",
		date_of_birth = "17/5/1032",
		address_line_1 = "1, Arniston Road",
		address_line_2 = "",
		city = "Zarlotep",
		state = "Oregon",
		country = "USA",
		postal_code = "1234",
		email_address = "alf@vega.com",
		account_login = "alf",
		account_password = "hell0Brian",
		security_question = "Best friend?",
		security_answer = "Brian"
		
	},

	?test_info_fmt( "Registering a first user: ~s.",
		[ orge_database_manager:user_settings_to_string(AlfSettings)] ),
		
	ServerPid ! {register_user,AlfSettings,self()},
	receive
		
		{registration_result,user_registered} ->
			?test_info( "First user settings successfully registered." )
		
	end,
	
	UnknownLogin = "Mr. Big",
	?test_info_fmt( "Unregistering now an unknown user whose login is: ~s.", 
					[UnknownLogin] ),
	
	ServerPid ! {unregister_user,UnknownLogin,self()},
	receive
		
		{unregistration_result,{user_unregistration_failed,FailureReason}} ->
			?test_info_fmt(
				"Unregistering of an unknown user failed as expected: ~w.",
				[FailureReason] )
		
	end,
	
	LukeSettings = #orge_user_settings{
		first_name = "Luke",
		last_name = "Skywalker",
		date_of_birth = "17/5/1977",
		address_line_1 = "1, avenue de la République",
		address_line_2 = "(chez les Jedi)",
		city = "Loué",
		state = "N/A",
		country = "France",
		postal_code = "34015",
		email_address = "luke@jedi.fr",
		account_login = "anakin",
		account_password = "Iamy0urfather",
		security_question = "What shall I do?",
		security_answer = "Use the Force"
	},
	
	?test_info_fmt( "Registering a second user: ~s.",
		[ orge_database_manager:user_settings_to_string(LukeSettings)] ),
		
	ServerPid ! {register_user,LukeSettings,self()},
	receive
		
		{registration_result,user_registered} ->
			?test_info( "Second user settings successfully registered." )
		
	end,
	
	
	% Corresponds to LukeSettings:
	?test_info( "Unregistering now first user." ),
	ServerPid ! {unregister_user,"anakin",self()},
	receive
		
		{unregistration_result,user_unregistered} ->
			?test_info( "Unregistering of a known user succeeded." )
		
	end,

	IndianaSettings = #orge_user_settings{
		first_name = "Indiana",
		last_name = "Jones",
		date_of_birth = "4/4/1957",
		address_line_1 = "University of California",
		address_line_2 = "",
		city = "San-Francisco",
		state = "California",
		country = "USA",
		postal_code = "ZIP 3239",
		email_address = "indiana@arch.org",
		account_login = "indiana",
		account_password = "museum",
		security_question = "Where does it belong?",
		security_answer = "That belongs in a museum. "
	},
	
	?test_info_fmt( "Registering a third user: ~s.",
		[ orge_database_manager:user_settings_to_string(IndianaSettings)] ),
		
	ServerPid ! {register_user,IndianaSettings,self()},
	receive
		
		{registration_result,user_registered} ->
			?test_info( "Third user settings successfully registered." )
		
	end,
	
	WaitForTest = true,
	
	case WaitForTest of
	
		true ->
			io:format( "   Launching the table viewer and "
				"waiting indefinitively.~n" ),
			tv:start(),
			receive
			
				Any ->
					?test_warning_fmt( "Test ignored following "
						"unexpected message: ~p.", [Any] )
					
			end;
			
		false ->
			ok	
	
	end,
	
	
	?test_info( "Requesting the server to shutdown." ),
	ServerPid ! {self(),shutdown},
	receive
		
		orge_server_shutdown ->
			?test_info( "Orge server successfully shutdown." )
			
	end,	
	?test_stop.


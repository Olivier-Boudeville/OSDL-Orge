% Unit tests for the Orge tcp_server.
% See the orge_tcp_server and orge_tcp_client_test modules.
-module(orge_tcp_server_test).



-define(test_server_name,my_orge_tcp_test_server).


-define(Tested_modules,[orge_tcp_server]).


% For orge_user_settings:
-include("orge_database_manager.hrl").

% For all facilities common to all tests:
-include("test_constructs.hrl").


	
run() ->

	?test_start,
	
	?test_info([ "Creating a new Orge tcp_server." ]),
	
	ServerPid = orge_tcp_server:create_link( ?test_server_name, from_scratch,
		local_only ),
		
	?test_info([ "Requesting server informations." ]),
	ServerPid ! {self(),get_info},	
	receive
	
		{server_info,StateString} ->
			?test_info([ io_lib:format( "Current server state is: ~s.",
				[StateString] ) ]) 
			
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

	?test_info([ "Registering a first user." ]),
	ServerPid ! {register_user,AlfSettings,self()},
	receive
		
		{registration_result,user_registered} ->
			?test_info([ "First user settings successfully registered." ])
		
	end,
	
	?test_info([ "Unregistering an unknown user." ]),
	UnknownLogin = "Mr. Big",
	
	ServerPid ! {unregister_user,UnknownLogin,self()},
	receive
		
		{unregistration_result,{user_unregistration_failed,FailureReason}} ->
			?test_info([ io_lib:format( 
				"Unregistering of an unknown user failed as expected: ~w.",
				[FailureReason] ) ])
		
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
	
	?test_info([ "Registering a second user." ]),
	ServerPid ! {register_user,LukeSettings,self()},
	receive
		
		{registration_result,user_registered} ->
			?test_info([ "Second user settings successfully registered." ])
		
	end,
	
	
	
	receive
	
		Any ->
			?test_info([ io_lib:format( "Test server received ~w", [Any] ) ])
			
	end,


	?test_info([ "Unregistering first user." ]),
	ServerPid ! {unregister_user,"anakin",self()},
	receive
		
		{unregistration_result,user_unregistered} ->
			?test_info([ "Unregistering of a known user succeeded." ])
		
	end,

	
	
	?test_info([ "Requesting the server to shutdown." ]),
	ServerPid ! {self(),shutdown},
	receive
		
		orge_server_shutdown ->
			?test_info([ "Orge server successfully shutdown." ])
			
	end,	
	?test_stop.


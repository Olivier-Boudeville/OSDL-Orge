% Unit tests for the tcp_server.
% See the tcp_server module.
% Uses an external test functional module with an intended bug in it
% (faulty_functional_module) and replaces it by a corrected one
% (corrected_test_functional_module).
-module(tcp_server_test).



-define(test_server_name,tcp_test_server).

-define(test_client_name,tcp_test_client).

-define(test_login,"Atanator").
-define(test_password,"l0vefromearth").

-define(Tested_modules,[tcp_server]).



% For all facilities common to all tests:
-include("test_constructs.hrl").


	
run() ->

	?test_start,
	
	?test_info([ "Creating a new tcp_server." ]),
	
	ServerPid = tcp_server:create_link( ?test_server_name, local_only, 
		client_manager ),
		
	?test_info([ "Requesting server informations." ]),
	ServerPid ! {self(),get_info},	
	receive
	
		{server_info,StateString} ->
			?test_info([ io_lib:format( "Current server state is: ~s.",
				[StateString] ) ]) 
			
	end,
	
	?test_info([ "Requesting the server to stop." ]),
	ServerPid ! close,
	 	
	?test_stop.


% Unit tests for the Orge tcp_server.
% See the orge_tcp_server and orge_tcp_client_test modules.
-module(orge_tcp_server_test).



-define(test_server_name,my_orge_tcp_test_server).


-define(Tested_modules,[orge_tcp_server]).



% For all facilities common to all tests:
-include("test_constructs.hrl").


	
run() ->

	?test_start,
	
	?test_info([ "Creating a new Orge tcp_server." ]),
	
	ServerPid = orge_tcp_server:create_link( ?test_server_name, local_only, 
		client_manager ),
		
	?test_info([ "Requesting server informations." ]),
	ServerPid ! {self(),get_info},	
	receive
	
		{server_info,StateString} ->
			?test_info([ io_lib:format( "Current server state is: ~s.",
				[StateString] ) ]) 
			
	end,
	
	receive
	
		Any ->
			?test_info([ io_lib:format( "Test server received ~w", [Any] ) ])
			
	end,
	?test_info([ "Requesting the server to shutdown." ]),
	ServerPid ! {self(),shutdown},
	 	
	?test_stop.


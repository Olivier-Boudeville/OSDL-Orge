% Custom-made test TCP/IP Orge client.
% See orge_tcp_server.erl and orge_tcp_server_test.erl.
-module(orge_tcp_client_test).


-define(Tested_modules,[orge_tcp_client]).



% For all facilities common to all tests:
-include("test_constructs.hrl").



	
run() ->

	?test_start,
	
	?test_info([ "A test Orge TCP server is expected to run already, "
		"with default settings (see orge_tcp_server_test.erl)." ]),

	?test_info([ "Creating a new Orge test TCP client." ]),
	
	ClientPid = orge_tcp_client:start_link( "My login", "My Password", 
		localhost ),
	
	timer:sleep(2000),
	
	%?test_info([ "Requesting server informations." ]),
	%ServerPid ! {self(),get_info},	
	%receive
	
	%	{server_info,StateString} ->
	%		?test_info([ io_lib:format( "Current server state is: ~s.",
	%			[StateString] ) ]) 
			
	%end,
	
	%?test_info([ "Requesting the server to shutdown." ]),
	%ServerPid ! {self(),shutdown},
	 	
	?test_stop.


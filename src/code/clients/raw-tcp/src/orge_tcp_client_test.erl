% Custom-made test TCP/IP Orge client.
% See orge_tcp_server.erl and orge_tcp_server_test.erl.
-module(orge_tcp_client_test).


-define(Tested_modules,[orge_tcp_client]).



% For all facilities common to all tests:
-include("test_constructs.hrl").



	
run() ->

	ServerLocation = localhost,
	?test_start,
	
	?test_info([ "A test Orge TCP server is expected to run already, "
		"with default settings (see orge_tcp_server_test.erl)." ]),

	?test_info([ "Creating a first new Orge test TCP client." ]),
	FirstClient = orge_tcp_client:start_link( "anakin", "Iamy0urfather", 
		ServerLocation ),
	
	?test_info([ "Checking this client succeeded in connecting." ]),
	FirstClient ! {get_login_status,self()},
	receive
	
		login_success ->
			ok
		
	end,
	
	% Too long to implement on sockets:
	%?test_info([ "Retrieving first user settings." ]),
	%FirstClient ! {get_user_settings,self()},
	%receive
	%
	%	{user_settings,FirstSettings} ->
	%		?test_info([ io_lib:format( "Received first user settings: ~s.", 
	%			[orge_database_manager:orge_user_settings_to_string(
	%				FirstSettings)]	) ])
	%	
	%end,
	
	
	?test_info([ "Creating a second new Orge test TCP client." ]),
	SecondClient = orge_tcp_client:start_link( "alf", "hell0Brian", 
		ServerLocation ),
	
	?test_info([ "Checking this client succeeded in connecting." ]),
	SecondClient ! {get_login_status,self()},
	receive
	
		login_success ->
			ok
		
	end,
	
	%?test_info([ "Retrieving second user settings." ]),
	%SecondClient ! {get_user_settings,self()},
	%receive
	%
	%	{user_settings,SecondSettings} ->
	%		?test_info([ io_lib:format( "Received second user settings: ~s.", 
	%			[orge_database_manager:orge_user_settings_to_string(
	%				SecondSettings)]	) ])
	%	
	%end,
	
	%?test_info([ "Trying to log twice with the account of first client "
	%	"(should fail)." ]),
	%SameAsFirstClient = orge_tcp_client:start_link( "anakin", "Iamy0urfather", 
	%	ServerLocation ),
	
	%?test_info([ "Checking this client failed in connecting." ]),
	%SameAsFirstClient ! {get_login_status,self()},
	%receive
	%
	%	{login_failed,already_logged} ->
	%		?test_info([ "Second login spotted and rejected as expected." ])
	%				
	%end,
		
		
	
	
	%timer:sleep(2000),
	
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


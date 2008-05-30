% Unit tests for the tcp_server.
% See the tcp_server module.
% Uses an external test functional module.
-module(tcp_server_test).



-define(test_server_name,tcp_test_server).

-define(Tested_modules,[tcp_server]).


% Use our functional module dedicated to tests:
-define(functional_module,test_functional_module).


% For all facilities common to all tests:
-include("test_constructs.hrl").


	
run() ->

	?test_start,
	
	?test_info([ "Creating a new tcp_server." ]),

	% Creates a TCP server and uses an external functional module:
	ServerPid = tcp_server:start_link( ?test_server_name, local_only, 
		?functional_module ),
	
	functionally_initialized = tcp_server:send_request( ServerPid, get_info ),
	
	NewInfo = functionally_changed,
	
	ok = tcp_server:send_request( ServerPid, {set_info,NewInfo} ),
	
	NewInfo = tcp_server:send_request( ServerPid, get_info ),

	?test_info([ "tcp_server behaved correctly." ]),
	
	% Uncomment to test failures and transactions:
	%?test_info([ io_lib:format( "Result of fail_on_purpose: ~w.", 
	%	[ tcp_server:send_request( ServerPid, fail_on_purpose ) ] ) ]),
		
	NewInfo = tcp_server:send_request( ServerPid, get_info ),
	
	?test_stop.


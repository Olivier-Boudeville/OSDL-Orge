% Unit tests for the tcp_server.
% See the tcp_server module.
% Uses an external test functional module with an intended bug in it
% (faulty_functional_module) and replaces it by a corrected one
% (corrected_test_functional_module).
-module(tcp_server_test).



-define(test_server_name,tcp_test_server).

-define(Tested_modules,[tcp_server]).



% For all facilities common to all tests:
-include("test_constructs.hrl").


	
run() ->

	?test_start,
	
	?test_info([ "Creating a new tcp_server, "
		"starting with faulty functional module." ]),

	% Creates a TCP server and uses an external functional module:
	ServerPid = tcp_server:start_link( ?test_server_name, local_only, 
		faulty_test_functional_module ),
	
	?test_info([ "tcp_server state: ~s.", [ 
		tcp_server:send_request( ServerPid, get_server_state ) ] ]),
	
	functionally_initialized = tcp_server:send_request( ServerPid, get_info ),
	
	NewInfo = functionally_changed,
	
	ok = tcp_server:send_request( ServerPid, {set_info,NewInfo} ),
	
	NewInfo = tcp_server:send_request( ServerPid, get_info ),

	?test_info([ "tcp_server behaved correctly." ]),
	
	{request_failed,Message} = tcp_server:send_request( ServerPid,
		fail_on_purpose ),
	
	?test_info([ io_lib:format( "Result of fail_on_purpose: ~w.", 
		[ Message ] ) ]),

	?test_info([ "Swapping functional implementations: "
		"replacing faulty one by corrected one." ]),
	
	functional_code_swapped = tcp_server:update_functional_code( ServerPid, 
		corrected_test_functional_module ),
		
	% Checks that code has been replaced:	
	ok = tcp_server:send_request( ServerPid, fail_on_purpose ),
	
	% Checks that the state was not lost:	
	NewInfo = tcp_server:send_request( ServerPid, get_info ),
	
	?test_stop.


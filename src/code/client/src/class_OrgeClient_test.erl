% Unit tests for the OrgeServer class implementation.
% See the class_OrgeServer module.
-module(class_OrgeServer_test).


-define(Tested_modules,[class_OrgeServer]).


% For all facilities common to all tests:
-include("test_constructs.hrl").


% For default_orge_server_tcp_port:
-include("class_OrgeServer.hrl").
			
			

% Run the tests in batch mode, no prior OrgeServer expected to be alive.
run() ->

	?test_start,
	
	TestServerName = orge_server_test_name,
	
	?test_info([ io_lib:format( "Creating a new OrgeServer named ~w "
		"that will listen to TCP port #~B.", 
		[TestServerName, ?default_orge_server_tcp_port] ) ]),
	
	TestOrgeServerPid = class_OrgeServer:new( TestServerName,
		?default_orge_server_tcp_port ),
	
	% Pattern matching:
	TestOrgeServerPid = case utils:wait_for_global_registration_of( 
			TestServerName ) of

		Answer when is_pid(Answer) ->
			Answer;
	
		Error ->
			testFailed( io_lib:format( "Unable to find target OrgeServer: ~w",
				[ Error ] ) )
	
	end,

	
	?test_info([ "Starting server." ]),
	TestOrgeServerPid ! start,
	
	WaitedSeconds = 2,

	?test_info([ io_lib:format("Sleeping for ~w seconds.",[WaitedSeconds]) ]),
	timer:sleep( WaitedSeconds * 1000 ),
	?test_info([ "End of sleep." ]),

	?test_info([ "Stopping server." ]),
	TestOrgeServerPid ! stop,
	
	?test_info([ "Removing server." ]),
	TestOrgeServerPid ! delete,

	?test_stop.


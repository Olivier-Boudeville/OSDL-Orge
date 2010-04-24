-module(class_MapSupervisor_test).


-define(Tested_modules,[class_MapSupervisor]).


% For trace facilities:
-include("traces_for_tests.hrl").


	
% Runs the test.
run() ->

	?test_start,

	TestBoundaries = { _TopLeft={-1000,2000}, _BottomRight={1500,-700} },
	
	TestWorld = class_VirtualWorld:synchronous_new_link( 
		"My Test Virtual World", TestBoundaries ),

	MapSupervisorPid = class_MapSupervisor:create_supervisor_for( TestWorld ),

	receive 

		t -> t 
	
	end,

	MapSupervisorPid ! delete,

	?test_stop.


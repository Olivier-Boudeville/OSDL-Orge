% Unit tests for the space module implementation.
% See the space module.
-module(space_test).


-define(Tested_modules,[space]).


% For all facilities common to all tests:
-include("test_constructs.hrl").

% For location:
-include("space.hrl").
	

% Run the tests.
run() ->

	?test_start,
	
	L1 = #location{ x = 1, y = 2, z = 3 },
	?test_info([ "First location:" ++ space:location_to_string(L1) ]),
	
	L2 = #location{ x = 0, y = -1.1, z = 300 },
	?test_info([ "Second location:" ++ space:location_to_string(L2) ]),

	?test_info([ io_lib:format( "Distance between these two locations: ~f.",
		[space:distance(L1,L2)] ) ]),

	L3 = #location{ x = 4, y = 5, z = 6 },

	% 9+9+9 = 27 = 5.196152^2
	27.0 = space:square_distance(L1,L3),
	
	?test_stop.


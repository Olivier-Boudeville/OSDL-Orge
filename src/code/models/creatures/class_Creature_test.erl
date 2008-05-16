% Unit tests for the Creature class implementation.
% See the class_Creature module.
-module(class_Creature_test).


-define(Tested_modules,[class_Creature]).


% For all facilities common to all tests:
-include("test_constructs.hrl").

% For location:
-include("space.hrl").
	

% Run the tests.
run() ->

	?test_start,
		
	?test_info([ "Creating a new Creature." ]),
	
	MyLocation = #location{ x = 1, y = 2, z = 3 },
		
	MyCreature = class_Creature:new( "Test creature", MyLocation ),		
	
	MyCreature ! delete,

	?test_stop.


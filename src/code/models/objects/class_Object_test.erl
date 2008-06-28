% Unit tests for the Object class implementation.
% See the class_Object module.
-module(class_Object_test).


-define(Tested_modules,[class_Object]).


% For all facilities common to all tests:
-include("test_constructs.hrl").

% For location:
-include("space.hrl").
	

% Run the tests.
run() ->

	?test_start,
		
	?test_info([ "Creating a new test Object." ]),
			
	Size = 0.8,
			
	MyObject = class_Object:new_link( "Bottle of Whisky", Size, 1.1, 120 ),	
	
	MyObject ! {getSize,[],self()},
	receive
	
		{wooper_result,Size} ->
				?test_info([ "getSize succeeded." ])

	end,
	
	NewSize = 0.9,
	
	MyObject ! {setSize,NewSize},
	
	MyObject ! {getSize,[],self()},
	receive
	
		{wooper_result,NewSize} ->
				?test_info([ "setSize succeeded." ])

	end,

	MyObject ! {toString,[],self()},
	receive
	
		{wooper_result,StateString} ->
				?test_info([ io_lib:format( 
					"Received following full textual description: ~s",
					[StateString] ) ])

	end,
	
	MyObject ! delete,

	?test_stop.


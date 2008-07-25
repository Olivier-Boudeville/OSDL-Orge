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
			
	MyObject = class_Object:new_link( "Bottle of Whisky", Size, 1.1, 
		555, 120 ),	
	
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
	
		{wooper_result,FirstStateString} ->
				?test_info([ io_lib:format( 
					"Received following full textual description: ~s",
					[FirstStateString] ) ])

	end,

	MyObject ! {isUsable,[],self()},
	receive
	
		{wooper_result,true} ->
				?test_info([ "Object is usable as expected." ])

	end,
	
	MyObject ! {isReparable,[],self()},
	receive
	
		{wooper_result,false} ->
				?test_info([ "Object is not reparable as expected." ])

	end,
	
	?test_info([ "Adding almost full wear for this object." ]),
	MyObject ! {increaseWearOf,500},
	
	MyObject ! {isUsable,[],self()},
	receive
	
		{wooper_result,false} ->
				?test_info([ "Object is no more usable, as expected." ])

	end,
	
	MyObject ! {isReparable,[],self()},
	receive
	
		{wooper_result,true} ->
				?test_info([ "Object is reparable now, as expected." ])

	end,
	MyObject ! {toString,[],self()},
	receive
	
		{wooper_result,SecondStateString} ->
				?test_info([ io_lib:format( 
					"Received following full textual description: ~s",
					[SecondStateString] ) ])

	end,
	
	MyObject ! delete,

	?test_stop.


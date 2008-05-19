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
	
	MyLocation = #location{ x = 1, y = 2, z = 3 },
		
	MyObject = class_Object:new_link( MyLocation ),		
	
	MyObject ! {getLocation,[],self()},
	
	receive
	
		{wooper_result,MyLocation} ->
				?test_info([ "getLocation succeeded." ])

	end,
	
	Origin = space:origin(),
	
	?test_info([ io_lib:format("Origin is ~w.", [ Origin ]) ]),
	
	MyObject ! {setLocation,Origin},
	
	MyObject ! {getLocation,[],self()},

	receive
	
		{wooper_result,Origin} ->
				?test_info([ "setLocation succeeded." ]);

		{wooper_result,Other} ->
				?test_error([ 
					io_lib:format("setLocation failed: ~w.", [ Other ]) ])

	end,
	
	Abscissa = 7,
	MyObject ! {setAbscissa,Abscissa},
	MyObject ! {getAbscissa,[],self()},
	
	receive
	
		{wooper_result,Abscissa} ->
				?test_info([ "setAbscissa and getAbscissa succeeded." ])

	end,

	Ordinate = 17,
	Altitude = 22,

	MyObject ! {setOrdinate,Ordinate},
	MyObject ! {setAltitude,Altitude},
	
	FinalLocation = #location{x=Abscissa,y=Ordinate,z=Altitude},

	MyObject ! {getLocation,[],self()},
	receive
	
		{wooper_result,FinalLocation} ->
				?test_info([ "set/get for abscissa, ordinate and altitude "
					"succeeded." ])

	end,
	
	MyObject ! delete,

	?test_stop.


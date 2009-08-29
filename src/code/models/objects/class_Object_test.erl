% 
% Copyright (C) 2003-2009 Olivier Boudeville
%
% This file is part of the Orge library.
%
% The Orge library is free software: you can redistribute it and/or modify
% it under the terms of either the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option) 
% any later version.
%
% The Orge library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License and of the GNU General Public License along with the Orge library.
% If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Unit tests for the Object class implementation.
% See the class_Object module.
-module(class_Object_test).


-define(Tested_modules, [class_Object] ).


% For all facilities common to all tests:
%-include("test_constructs.hrl").

% For trace facilities:
-include("traces_for_tests.hrl").


	

% Run the tests.
run() ->

	?test_start,
		
	?test_info([ "Creating a new test Object." ]),
			
	Size = 0.8,
			
	MyObject = class_Object:synchronous_new_link( "Bottle of Whisky", Size, 
		1.1, 555, 120 ),	
	
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


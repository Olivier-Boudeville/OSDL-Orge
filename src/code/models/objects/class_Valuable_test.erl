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


% Unit tests for the Valuable class implementation.
% See the class_Valuable module.
-module(class_Valuable_test).


-define(Tested_modules, [class_Valuable] ).


% For all facilities common to all tests:
%-include("test_constructs.hrl").

% For trace facilities:
-include("traces_for_tests.hrl").
	
	

% Run the tests.
run() ->

	?test_start,
		
	?test_info([ "Creating a new Valuable." ]),
	
		
	MyValuable = class_Valuable:synchronous_new_link( _BaseValue = 32 ),		
	
	MyValuable ! delete,

	?test_stop.


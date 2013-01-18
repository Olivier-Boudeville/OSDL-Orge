% Copyright (C) 2003-2013 Olivier Boudeville
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
-module(entity_test).


-define(Tested_modules, [entity] ).

% As an example, a simple non-abstract entity is:
-record( my_test_entity, {

		   % The entity location, {X,Y}, both being in-world integer
		   % coordinates:
		   location = {0,0},

		   my_other_field = 1

		  }

).

% For trace facilities:
-include("traces_for_tests.hrl").


% For entity macros:
-include("entity.hrl").

?define_translate_for(abstract_entity);
?define_translate_for(my_test_entity).


% Run the tests.
run() ->

	?test_start,

	?test_info( "Creating a new test entity." ),

	MyEntity = #my_test_entity{},

	?test_info_fmt( "Initial test entity is ~w.", [MyEntity] ),

	MyTranslatedEntity = translate( MyEntity, {7,11} ),

	?test_info_fmt( "Translated test entity is ~w.", [MyTranslatedEntity] ),

	?test_stop.

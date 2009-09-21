% 
% Copyright (C) 2009 Olivier Boudeville
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


% Unit tests for the Creature class implementation.
% See the class_Creature module.
-module(class_LanguageManager_test).


-define(Tested_modules, [class_LanguageManager] ).


% For trace facilities:
-include("traces_for_tests.hrl").


% Run the tests.
run() ->

	?test_start,

	Language = 	"modern-greek",
	Variation = 'female-names',
	Options = [ generate_original_only ],
		
	?test_info([ io_lib:format( "Creating a new language manager, "
		"for variation '~s' of language '~s'.", [ Language, Variation ] ) ]),
	
	MyManager = class_LanguageManager:synchronous_new_link( Language,
		Variation, _MarkovOrder = 2, Options ),		
	
	MyManager ! learn,
	
	MyManager ! {generate,[Variation],self()},
	GeneratedWord = receive
	
		{wooper_result, {generation_success,Word} } ->
			?test_info([ io_lib:format( 
				"Generated word for variation '~s' is '~s'.", [Variation,Word] )
			])
	
	end,
	
	MyManager ! {evaluate,[GeneratedWord,Variation],self()},
	receive
	
		{wooper_result,GeneratedWordProbability} ->
			?test_info([ io_lib:format( "The evaluated probability that "
				"the generated word '~s' belongs to variation '~s' is ~f%.",
				[Word,Variation,GeneratedWordProbability*100] )
			])
	
	end,
	
	
	MyManager ! delete,

	?test_stop.


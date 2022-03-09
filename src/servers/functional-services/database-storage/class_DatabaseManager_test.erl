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


% Unit tests for the Orge database manager.
% See the class_DatabaseManager module.
-module(class_DatabaseManager_test).


% For orge_user record:
-include("class_DatabaseManager.hrl").

-define(Tested_modules, [class_DatabaseManager] ).



% For trace facilities:
-include("traces_for_tests.hrl").


compose_user_list([]) ->
	"empty user list";

compose_user_list(Users) ->
	compose_user_list(Users,[]).
	
	

compose_user_list([],Acc) ->
	Acc;
	
compose_user_list([H|T],Acc) ->
	compose_user_list(T, Acc ++ io_lib:format( "~n  * ", [] ) 	
		++ class_DatabaseManager:orge_user_to_string(H) ).
	
	
	
run() ->

	?test_start,
	
	?test_info( "Creating a blank database." ),
	BasePid = class_DatabaseManager:new_link( from_scratch,
					   _MaximumSlotCount=128, no_listener ),

	AlfSettings = #orge_user_settings{
		first_name = "Gordon",
		last_name = "Shumway",
		date_of_birth = "17/5/1032",
		address_line_1 = "1, Arniston Road",
		address_line_2 = "",
		city = "Zarlotep",
		state = "Oregon",
		country = "USA",
		postal_code = "1234",
		email_address = "alf@vega.com",
		account_login = "alf",
		account_password = "hell0Brian",
		security_question = "Best friend?",
		security_answer = "Brian"
	},
	
	LukeSettings = #orge_user_settings{
		first_name = "Luke",
		last_name = "Skywalker",
		date_of_birth = "17/5/1977",
		address_line_1 = "1, avenue de la République",
		address_line_2 = "(chez les Jedi)",
		city = "Loué",
		state = "N/A",
		country = "France",
		postal_code = "34015",
		email_address = "luke@jedi.fr",
		account_login = "anakin",
		account_password = "Iamy0urfather",
		security_question = "What shall I do?",
		security_answer = "Use the Force"	
	},
	
	?test_info( "Requesting the users of the blank Orge database." ),
	BasePid ! {listAllUsers,[],self()},
	receive
	
		{wooper_result,EmptyList} ->
			?test_info_fmt( "Returned users: ~s.", 
							[compose_user_list(EmptyList)] ) 
			
	end,
	
	?test_info_fmt( "Feeding it with first user settings: ~s.", 
		[class_DatabaseManager:orge_user_settings_to_string(AlfSettings)] ),

	BasePid ! {registerUser,AlfSettings,self()},
	receive
		
		{wooper_result,register_success} ->
			?test_info( "First user settings successfully registered." )
		
	end,
	
	?test_info_fmt( "Feeding it with second user settings: ~s.", 
		[class_DatabaseManager:orge_user_settings_to_string(LukeSettings)] ),

	BasePid ! {registerUser,LukeSettings,self()},
	receive
		
		{wooper_result,register_success} ->
			?test_info( "Second user settings successfully registered." )
		
	end,

	?test_info( "Trying to register twice the first user." ),
	BasePid ! {registerUser,AlfSettings,self()},
	receive
		
		{wooper_result,{register_failed,login_already_registered}} ->
			?test_info( "First user successfully rejected "
						"as already registered." )
		
	end,
	
	?test_info( "Requesting the users of the updated Orge database." ),
	BasePid ! {listAllUsers,[],self()},
	receive
	
		{wooper_result,UpdatedList} ->
			?test_info_fmt( "Returned users: ~s.", 
							[compose_user_list(UpdatedList)])
			
	end,
	
	
	?test_info( "Stopping the database." ),
	BasePid ! {stop,[],self()},
	receive
	
		{wooper_result,orge_database_stopped} ->
			ok
			
	end,
			
	?test_info( "Reloading it with previous state." ),
	NewBasePid = class_DatabaseManager:new_link( from_previous_state,
				_MaximumSlotCount=128, no_listener ),


	?test_info( "Requesting the users of the updated Orge database." ),
	NewBasePid ! {listAllUsers,[],self()},
	receive
	
		{wooper_result,ReloadedList} ->
			?test_info_fmt( "Returned users: ~s.",
				[compose_user_list(ReloadedList)] )
			
	end,

	?test_info( "Stopping the database once again." ),
	NewBasePid ! {stop,[],self()},
	receive
	
		{wooper_result,orge_database_stopped} ->
			ok
			
	end,

	?test_stop.


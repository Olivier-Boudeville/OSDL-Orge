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


% Unit tests for the Orge database manager.
% See the orge_database_manager module.
-module(orge_database_manager_test).


% For orge_user record:
-include("orge_database_manager.hrl").

-define(Tested_modules,[orge_database_manager]).



% For all facilities common to all tests:
-include("test_constructs.hrl").

compose_user_list([]) ->
	"empty user list";

compose_user_list(Users) ->
	compose_user_list(Users,[]).
	

compose_user_list([],Acc) ->
	Acc;
	
compose_user_list([H|T],Acc) ->
	compose_user_list(T, Acc ++ io_lib:format( "~n  * ", [] ) 	
		++ orge_database_manager:orge_user_to_string(H) ).
	
	
	
run() ->

	?test_start,
	
	?test_info([ "Creating a blank database." ]),
	BasePid = orge_database_manager:start(from_scratch),

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
	
	?test_info([ "Requesting the users of the blank Orge database." ]),
	BasePid ! {list_users,self()},
	receive
	
		{orge_users,EmptyList} ->
			?test_info([ io_lib:format( "Returned users: ~s.", 
				[compose_user_list(EmptyList)] ) ]) 
			
	end,
	
	?test_info([ io_lib:format( "Feeding it with first user settings: ~s.", 
		[orge_database_manager:orge_user_settings_to_string(AlfSettings)] ) ]),
	BasePid ! {register_user,AlfSettings,self()},
	receive
		
		register_success ->
			?test_info([ "First user settings successfully registered." ])
		
	end,
	
	?test_info([ io_lib:format( "Feeding it with second user settings: ~s.", 
		[orge_database_manager:orge_user_settings_to_string(LukeSettings)] ) ]),
	BasePid ! {register_user,LukeSettings,self()},
	receive
		
		register_success ->
			?test_info([ "Second user settings successfully registered." ])
		
	end,

	?test_info([ "Trying to register twice the first user." ]),
	BasePid ! {register_user,AlfSettings,self()},
	receive
		
		{register_failed,login_already_registered} ->
			?test_info([ "First user successfully rejected "
				"as already registered." ])
		
	end,
	
	?test_info([ "Requesting the users of the updated Orge database." ]),
	BasePid ! {list_users,self()},
	receive
	
		{orge_users,UpdatedList} ->
			?test_info([ io_lib:format( "Returned users: ~s.", 
				[compose_user_list(UpdatedList)]) ]) 
			
	end,
	
	
	?test_info([ "Stopping the database." ]),
	BasePid ! {stop,self()},
	receive
	
		orge_database_stopped ->
			ok
			
	end,
			
	?test_info([ "Reloading it with previous state." ]),
	NewBasePid = orge_database_manager:start(from_previous_state),

	?test_info([ "Requesting the users of the updated Orge database." ]),
	NewBasePid ! {list_users,self()},
	receive
	
		{orge_users,ReloadedList} ->
			?test_info([ io_lib:format( "Returned users: ~s.",
				[compose_user_list(ReloadedList)]) ])
			
	end,

	?test_info([ "Stopping the database once again." ]),
	NewBasePid ! {stop,self()},
	receive
	
		orge_database_stopped ->
			ok
			
	end,

	?test_stop.


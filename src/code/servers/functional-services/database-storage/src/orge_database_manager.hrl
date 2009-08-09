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




% Describes the Mnesia entry type (fields) for Orge user settings:
-record( orge_user_settings, 
	{
	
		% The first name of the user (ex: "John")
		first_name,
		
		% The last name of the user (ex: "Wood")
		last_name,
		
		% The date of birth of the user (ex: {17,5,1971}): 
		date_of_birth,
		
		% The first line of the address of the user (ex: "1, Main Street"):
		address_line_1,
		
		% The second line of the address of the user (ex: "Second floor"):
		address_line_2,
		
		% The city in the address of the user (ex: "Bleston"):
		city,
		
		% The state/province in the address of the user (ex: "Oregon"):
		state,
		
		% The country in the address of the user (ex: "USA"):
		country,
		
		% The postal code of the address of the user (ex: "92310"):
		postal_code,
		
		% The phone number of the user, at home (ex: "+33146890265"): 
		home_phone,
		
		% The mobile number of the user (ex: "+33616833723"): 
		mobile_phone,
		
		% The e-mail address of the user (ex: "john.wood@example.com"):
		email_address,
		
		% The Orge login of the user (ex: "lonewolf_71"):
		account_login,
		
		% The Orge passwor of the user (ex: "suzannarocks"):
		account_password,
		
		% The user-specified question that the user, and only him, should 
		% be able to answer (ex: "What is my favorite meal?"):
		security_question,
		
		% The corresponding answer (ex: "French fries with garlic"):
		security_answer
		
	}
).
 

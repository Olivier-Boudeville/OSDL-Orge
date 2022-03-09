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


% Unit tests for the currency implementation.
% See the currency module.
-module(currency_test).


-define(Tested_modules, [currency] ).


% For trace facilities:
-include("traces_for_tests.hrl").



convertCoinsToCredits( GoldCoinCount, SilverCoinCount, CopperCoinCount ) ->

	Credits = currency:coins_to_credits( GoldCoinCount,
		SilverCoinCount, CopperCoinCount ), 

	?test_info_fmt( "~B gold coin(s), ~B silvers coin(s), "
		"~B copper coin(s) correspond(s) to ~B credit(s).", 
		[ GoldCoinCount, SilverCoinCount, CopperCoinCount, Credits ] ),

	Credits.
			
			
convertCreditsToCoins( Credits ) ->
	{Gold,Silver,Copper} = currency:credits_to_coins( Credits ),

	?test_info_fmt( "~B credit(s) correspond(s) to ~B gold coin(s),"
		" ~B silvers coin(s), ~B copper coin(s).", 
		[ Credits, Gold, Silver, Copper ] ),

	{Gold,Silver,Copper}.	 
	


% Run the tests.
run() ->

	?test_start,
	
	1    = convertCoinsToCredits( 0, 0, 1 ),
	20   = convertCoinsToCredits( 0, 1, 0 ),
	1000 = convertCoinsToCredits( 1, 0, 0 ),
	1021 = convertCoinsToCredits( 1, 1, 1 ),

	{0,0,1} = convertCreditsToCoins( 1 ),
	{0,1,0} = convertCreditsToCoins( 20 ),
	{1,0,0} = convertCreditsToCoins( 1000 ),
	{1,1,1} = convertCreditsToCoins( 1021 ),

	?test_stop.


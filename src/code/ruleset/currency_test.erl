% Unit tests for the currency implementation.
% See the currency module.
-module(currency_test).


-define(Tested_modules,[currency]).


% For all facilities common to all tests:
-include("test_constructs.hrl").


convertCoinsToCredits( GoldCoinCount, SilverCoinCount, CopperCoinCount ) ->
	Credits = currency:coins_to_credits( GoldCoinCount,
		SilverCoinCount, CopperCoinCount ), 
	?test_info([ io_lib:format( "~B gold coin(s), ~B silvers coin(s), "
		"~B copper coin(s) correspond(s) to ~B credit(s).", 
		[ GoldCoinCount, SilverCoinCount, CopperCoinCount, Credits ] ) ]),
	Credits.
			
			
convertCreditsToCoins( Credits ) ->
	{Gold,Silver,Copper} = currency:credits_to_coins( Credits ),
	?test_info([ io_lib:format( "~B credit(s) correspond(s) to ~B gold coin(s),"
		" ~B silvers coin(s), ~B copper coin(s).", 
		[ Credits, Gold, Silver, Copper ] ) ]),
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


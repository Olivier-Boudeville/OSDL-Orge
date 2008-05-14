% Allows to manage in-world currencies and money conversions.
% Internally all money values (amounts) are expressed in Orge credits.
-module(currency).


-export([coins_to_credits/1,coins_to_credits/2,coins_to_credits/3,
	credits_to_coins/1]).



% Note: always protects the macros with parentheses.

% 1 copper coin corresponds to 1 credit:
-define(copper_factor,1).

% 1 silver coin corresponds to 20 copper coins:
-define(silver_factor,(20*?copper_factor)).

% 1 gold coin corresponds to 50 silver coins:
-define(gold_factor,(50*?silver_factor*?copper_factor)).



coins_to_credits(CopperCoinCount) ->
	CopperCoinCount * ?copper_factor.

coins_to_credits(SilverCoinCount,CopperCoinCount) ->
	SilverCoinCount * ?silver_factor + coins_to_credits(CopperCoinCount).


coins_to_credits(GoldCoinCount,SilverCoinCount,CopperCoinCount) ->
	GoldCoinCount * ?gold_factor 
		+ coins_to_credits(SilverCoinCount,CopperCoinCount).


credits_to_coins(Credits) ->
	GoldCoins        = Credits div ?gold_factor,
	RemainingCredits = Credits rem ?gold_factor,
	SilverCoins = RemainingCredits div ?silver_factor,
	LastCredits = RemainingCredits rem ?silver_factor,
	CopperCoins = LastCredits div ?copper_factor,
	{GoldCoins,SilverCoins,CopperCoins}.
	
	

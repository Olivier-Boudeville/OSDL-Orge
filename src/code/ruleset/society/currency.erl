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
	
	

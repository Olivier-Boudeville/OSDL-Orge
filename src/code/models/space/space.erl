-module(space).


-export([distance/2,square_distance/2,location_to_string/1]).


% For location:
-include("space.hrl").


% Returns the distance between the two specified locations.
distance(L1,L2) ->
	math:sqrt( square_distance(L1,L2) ).


% Returns the square of the distance between the two specified locations.
% Note: faster than distance/2.
square_distance(L1,L2) ->
	#location{x=L1x, y=L1y, z=L1z} = L1,
	#location{x=L2x, y=L2y, z=L2z} = L2,
	math:pow(L1x-L2x,2) + math:pow(L1y-L2y,2) + math:pow(L1z-L2z,2).


% Returns a textual description of specified location.
location_to_string(Location) ->
	% Left to ~w instead of ~f or ~B to allow for both integers and floats:
	io_lib:format( "(~w;~w;~w)", [ Location#location.x, Location#location.y,
		Location#location.z ] ).



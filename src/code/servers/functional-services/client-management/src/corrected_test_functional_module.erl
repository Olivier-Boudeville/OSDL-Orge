% Copyright (C) 2003-2010 Olivier Boudeville
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


% Alternate functional module defined for test purposes, notably to check that
% functional modules can be swapped during execution (hot reload).
%
% Allows to have functional services separated from various server
% implementations, such as raw-tcp and al.
%
% Made to replace the faulty_test_functional_module module.
-module(corrected_test_functional_module).


% This functional API:
-export([ init/0, handle/2 ]).

-record( functional_state, {an_info} ).


% For trace sending from tests:
-include("traces.hrl").


init() ->
	?notify_info( "Corrected functional module initialized." ),
	#functional_state{an_info = functionally_initialized}.
	

% handle/2 must return {Answer,NewFunctionalState}:
	
handle( get_info, FunctionalState ) ->
	?notify_info( "get_info called from corrected module." ),
	{FunctionalState#functional_state.an_info,FunctionalState} ;
	
handle( {set_info,NewInfo}, FunctionalState ) ->
	?notify_info( "set_info called from corrected module." ),
	{ok,FunctionalState#functional_state{an_info=NewInfo}};
	
handle( fail_on_purpose, FunctionalState ) ->
	?notify_info( "fail_on_purpose called from corrected module." ),
	% This version is supposed to be corrected, thus it does not crash:
	{ok,FunctionalState}.
	
	

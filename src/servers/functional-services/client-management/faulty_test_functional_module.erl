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


% Functional module defined for test purposes.
%
% Allows to have functional services separated from various server
% implementations, such as raw-tcp and al.
%
% This version has a voluntary bug in the 'fail_on_purpose' function.
%
% Made to be replaced during execution by a fixed module,
% correct_test_functional_module.
-module(faulty_test_functional_module).


% This functional API:
-export([ init/0, handle/2 ]).

-record( functional_state, {an_info} ).


% For trace sending from tests:
-include("traces.hrl").


init() ->
	?notify_info( "Faulty functional module initialized." ),
	#functional_state{an_info = functionally_initialized}.
	

% handle/2 must return {Answer,NewFunctionalState}:
	
handle( get_info, FunctionalState ) ->
	?notify_info( "get_info called from faulty module." ),
	{FunctionalState#functional_state.an_info,FunctionalState} ;
	
handle( {set_info,NewInfo}, FunctionalState ) ->
	?notify_info( "set_info called from faulty module." ),
	{ok,FunctionalState#functional_state{an_info=NewInfo}};
	
handle( fail_on_purpose, FunctionalState ) ->
	?notify_info( "fail_on_purpose called from faulty module." ),
	FunctionalState = cannot_be_matched,
	{ok,FunctionalState}.
	
	


% Helper section for client routines disabled, as deemed less clear than direct
% calls like 'send_request( ServerPid, get_info )'.
%
% ('static')
%
%get_info( ServerPid ) ->
%	send_request( ServerPid, get_info ).
%	
%set_info(NewInfo) ->	
%	send_request( ServerPid, {set_info,NewInfo} ).
%
%fail_on_purpose() ->
%	send_request( ServerPid, fail_on_purpose).
	

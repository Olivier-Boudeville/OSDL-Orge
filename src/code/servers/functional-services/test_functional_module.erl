% Functional module defined for test purposes.
% Allows to have functional services separated from various server 
% implementations, such as raw-tcp and al.
-module(test_functional_module).


% This functional API:
-export([init/0,handle/2]).

-record( functional_state, {an_info} ).


% For trace sending from tests:
-include("test_constructs.hrl").


init() ->
	?test_info([ "Functional module initialized." ]),
	#functional_state{an_info = functionally_initialized}.
	

% handle/2 must return {Answer,NewFunctionalState}:
	
handle( get_info, FunctionalState ) ->
	?test_info([ "get_info called." ]),
	{FunctionalState#functional_state.an_info,FunctionalState} ;
	
handle( {set_info,NewInfo}, FunctionalState ) ->
	?test_info([ "set_info called." ]),
	{ok,FunctionalState#functional_state{an_info=NewInfo}};
	
handle( fail_on_purpose, FunctionalState ) ->
	?test_info([ "fail_on_purpose called." ]),
	FunctionalState = cannot_be_matched,
	{ok,FunctionalState}.
	
	


% Helper section for client routines disabled, as deemed less clear than direct % calls like 'send_request( ServerPid, get_info )'.
% ('static')
%get_info( ServerPid ) ->
%	send_request( ServerPid, get_info ).
%	
%set_info(NewInfo) ->	
%	send_request( ServerPid, {set_info,NewInfo} ).
%
%fail_on_purpose() ->
%	send_request( ServerPid, fail_on_purpose).
	

% Functional module defined for test purposes.
% Allows to have functional services separated from various server 
% implementations, such as raw-tcp and al.
% This version has a voluntary bug in the 'fail_on_purpose' function.
% Made to be replaced during execution by a fixed module,
% correct_test_functional_module.
-module(faulty_test_functional_module).


% This functional API:
-export([init/0,handle/2]).

-record( functional_state, {an_info} ).


% For trace sending from tests:
-include("traces.hrl").


init() ->
	?emit_info([ "Faulty functional module initialized." ]),
	#functional_state{an_info = functionally_initialized}.
	

% handle/2 must return {Answer,NewFunctionalState}:
	
handle( get_info, FunctionalState ) ->
	?emit_info([ "get_info called from faulty module." ]),
	{FunctionalState#functional_state.an_info,FunctionalState} ;
	
handle( {set_info,NewInfo}, FunctionalState ) ->
	?emit_info([ "set_info called from faulty module." ]),
	{ok,FunctionalState#functional_state{an_info=NewInfo}};
	
handle( fail_on_purpose, FunctionalState ) ->
	?emit_info([ "fail_on_purpose called from faulty module." ]),
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
	

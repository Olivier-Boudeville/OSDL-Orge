% Alternate functional module defined for test purposes, notably to check that
% functional modules can be swapped during execution (hot reload).
% Allows to have functional services separated from various server 
% implementations, such as raw-tcp and al.
% Made to replace the faulty_test_functional_module module.
-module(corrected_test_functional_module).


% This functional API:
-export([init/0,handle/2]).

-record( functional_state, {an_info} ).


% For trace sending from tests:
-include("traces.hrl").


init() ->
	?emit_info([ "Corrected functional module initialized." ]),
	#functional_state{an_info = functionally_initialized}.
	

% handle/2 must return {Answer,NewFunctionalState}:
	
handle( get_info, FunctionalState ) ->
	?emit_info([ "get_info called from corrected module." ]),
	{FunctionalState#functional_state.an_info,FunctionalState} ;
	
handle( {set_info,NewInfo}, FunctionalState ) ->
	?emit_info([ "set_info called from corrected module." ]),
	{ok,FunctionalState#functional_state{an_info=NewInfo}};
	
handle( fail_on_purpose, FunctionalState ) ->
	?emit_info([ "fail_on_purpose called from corrected module." ]),
	% This version is supposed to be corrected, thus it does not crash:
	{ok,FunctionalState}.
	
	

% Unit tests for the tcp_server.
% See the tcp_server module.
-module(tcp_server_test).



-define(test_server_name,tcp_test_server).


% Section for functional module emulation.
-record(functional_state,{an_info}).

% This functional API:
-export([init/0,handle/2]).

-define(Tested_modules,[tcp_server]).


% For all facilities common to all tests:
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
	{ok,FunctionalState#functional_state{an_info=NewInfo}}.
	
	


% Helper section for client routines.
get_info() ->
	tcp_server:send_request(?test_server_name,get_info).
	
set_info(NewInfo) ->	
	tcp_server:send_request( ?test_server_name, {set_info,NewInfo} ).




run() ->

	?test_start,
	
	?test_info([ "Creating a new tcp_server." ]),

	% Creates a TCP server and uses this test module as functional module:
	tcp_server:start_link( ?test_server_name, local_only, ?MODULE ),
	
	functionally_initialized = get_info(),
	
	ok = set_info( functionally_changed ),
	
	functionally_changed = get_info(),

	?test_info([ "tcp_server behaved correctly." ]),
	
	?test_stop.


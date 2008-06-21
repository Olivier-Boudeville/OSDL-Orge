% Server-side module dedicated to the management of a client connection.
% Features:
%  - functional behaviour (client management) uncoupled from server
% implementation
%  - transaction semantics
-module(client_manager).

-export([init/2]).


% For manager_state:
-include("client_manager.hrl").


% For emit_*:
-include("traces.hrl").


% Inits the client socket.
init(ServerPid,ListenSocket) ->
	?emit_debug([ io_lib:format( 
		"Client manager ~w waiting for next connection.", [self()] ) ]),
		
	% This process will be the one controlling the client socket once created:	
	{ok,ClientSocket} = gen_tcp:accept(ListenSocket),
	?emit_debug([ "Connection accepted." ]),
	ServerPid ! {self(),accepted},
	
	ManagerState = #manager_state{
	    starting_time = utils:get_timestamp(),
		client_socket = ClientSocket
	},
	loop( ManagerState ).
	
		
% Main loop of this client manager.
% ManagerState is the current state of this manager (ex: containing the name
% under which the server is registered and all other informations).
loop( ManagerState ) ->
	receive
	
		Any ->
			?emit_debug([ io_lib:format( 
				"Manager received: ~w.", [Any] ) ])
			
	end,
	loop( ManagerState ).

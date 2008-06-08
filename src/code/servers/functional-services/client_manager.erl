% Server-side module dedicated to the management of a client connection.
% Features:
%  - functional behaviour (client management) uncoupled from server
% implementation
%  - transaction semantics
-module(client_manager).

-export([init/1]).


% Inits the client socket.
init(ServerPid,ListenSocket) ->
	?emit_debug([ io_lib:format( 
		"Client manager ~w waiting for next connection.", [self()] ) ]),
		
	% This process will be the one controlling the client socket once created:	
	{ok,ClientSocket} = gen_tcp:accept(ListenSocket),
	?emit_debug([ "Connection accepted." ]),
	ServerPid ! 
		

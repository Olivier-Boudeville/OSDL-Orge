% Custom-made TCP/IP server.
% Features:
%  - functional behaviour uncoupled from server implementation
%  - multiple parallel sessions can be handled
%  - transaction semantics
%  - hot code update
%  - simple state management
%  - based on PID rather than on registered names
-module(tcp_server).



% Implementation notes.

% Main objective is to uncouple the technical server from the functional one,
% so that one can be modified with no impact on the other.
% Not using OTP gen_server, to learn how to do it and, maybe, to improve 
% performances, ease of integration or adequation to the Orge specific needs
% (ex: contrary to the usual client/server approach, the Orge server has to
% "push", i.e. send to the clients, simulation updates without waiting for a
% specific request to do so).
% Inspired from the various servers described in 'Programming Erlang',
% by Joe Armstrong (chapter 16).
% WOOPER has been avoided here to presumably speedup the processings, and 
% as no need for inheritance is expected.

% The role of these servers is simply to break incoming data stream into
% appropriate client requests, and to send them updates.


% For server defaults:
-include("tcp_server.hrl").


% For emit_*:
-include("traces.hrl").


% Record that stores the current state of a TCP server:
-record( server_state, {
	server_name = undefined,
	host = undefined,
	starting_time = undefined,
	code_update_count = 0,
	last_code_update = undefined,
	listening_socket = undefined,
	listening_port = undefined,
	current_connections = [],
	past_connections = []
} ).


% Exported API section.

-export([start/3,start_link/3,send_request/2,send_request_by_name/2,
	update_functional_code/2,state_to_string/1]).


% Starts a new TCP server.
%  - ServerName the name under which the server will be registered
%  - RegistrationType is in 'local_only', 'global_only', 'local_and_global', 
% depending on what kind of registration is requested for this server
%  - FunctionalModule the Erlang module that will manage the client requests
% and the pushed updates 
% Returns the PID of the launched server.
% (static)
start(ServerName,RegistrationType,FunctionalModule) ->
	?emit_trace([ io_lib:format( "Starting a TCP server named ~s.",
		[ ServerName ] ) ]),
	ServerPid = spawn( fun() ->	init( ServerName, FunctionalModule ) end ),
	ok = utils:register_as( ServerPid, ServerName, RegistrationType ),
	ServerPid.


% Starts a new TCP server, linked to the calling process.
%  - ServerName the name under which the server will be registered
%  - RegistrationType is in 'local_only', 'global_only', 'local_and_global', 
% depending on what kind of registration is requested for this server
%  - FunctionalModule the Erlang module that will manage the client requests
% and the pushed updates 
% Returns the PID of the launched server.
% (static)
start_link(ServerName,RegistrationType,FunctionalModule) ->
	?emit_trace([ io_lib:format( "Starting a linked TCP server named ~s.",
		[ ServerName ] ) ]),
	ServerPid = spawn_link( 
		fun() -> init( ServerName, FunctionalModule ) end ),
	ok = utils:register_as( ServerPid, ServerName,RegistrationType ),
	ServerPid.


% Initializes the TCP server with specified name and functional module, and
% enter its main loop.
% A default TCP port is used here.
init( ServerName, FunctionalModule ) ->
	init( ServerName, FunctionalModule, ?default_listening_tcp_server_port ).


% Initializes the TCP server with specified name, functional module and TCP
% port, and enter its main loop.
init( ServerName, FunctionalModule, ListeningTCPPort ) ->

	?emit_debug([ io_lib:format( "Server will listen to TCP port ~B.", 
		[ListeningTCPPort] ) ]),
		
	% All network interfaces listen to, 32-bit header:
	{ok,ListenSocket} = gen_tcp:listen( ListeningTCPPort, [
		binary, {packet,4}, {backlog,?default_backlog}, 
		{reuseaddr,true}, {active,true} ] ),

	?emit_debug([ "Server waiting for incoming connections." ]),
		
	% Will block until a connection happens:
	{ok,ClientSocket} = gen_tcp:accept(ListenSocket),
	
	?emit_debug([ "Connection accepted." ]),
		
	ServerState = #server_state{
		server_name = ServerName,
	    host = net_adm:localhost(),
	    starting_time = utils:get_timestamp(),
	    code_update_count = 0,
	    last_code_update = undefined,
		listening_socket = ListenSocket,
	    listening_port = ListeningTCPPort,
	    current_connections = [ClientSocket],
	    past_connections = []
	},
	FunctionalState = FunctionalModule:init(),
	loop( ServerState, FunctionalModule, FunctionalState ).
	

% Sends specified request to specified PID or name registered locally.
send_request( ServerPid, Request ) when is_pid(ServerPid) ->
	ServerPid ! {self(),Request},
	receive
	
		{ServerPid,Answer} ->
			Answer;

		{ServerPid,request_failed,Message} ->
			{request_failed,Message}		
	
	end.


send_request_by_name( ServerName, Request ) ->
	send_request( erlang:whereis(ServerName), Request ).
	

% Replaces, for specified server, current version of functional code by
% specified one.
update_functional_code(ServerPid,NewFunctionalModule) ->
	send_request( ServerPid, {update_functional_code,NewFunctionalModule} ).




% Internal API section.


% Main loop of this server.
%  - ServerState is the current state of this server (ex: containing the name
% under which the server is registered)
%  - FunctionalModule the Erlang module that manages the client requests
% and the pushed updates
%  - FunctionalState the state of the functional server 
loop( ServerState, FunctionalModule, FunctionalState ) ->
	?emit_debug([ io_lib:format( "~s~n", [ state_to_string(ServerState) ] ) ]),
	receive
	
		{tcp,ClientSocket,BinMessage} ->
			?emit_debug([ io_lib:format( "Server received binary ~p.~n",
				[BinMessage] ) ]),
			gen_tcp:send( ClientSocket, term_to_binary(packet_received) ),
			?emit_debug([ "Server answered.~n" ]),
			loop( ServerState, FunctionalModule, FunctionalState );

		{tcp_closed,_ClientSocket} ->
			?emit_trace([ "Client closed its socket.~n" ]);
		
		{From,{update_functional_code,NewFunctionalModule}} ->
			From ! {self(),functional_code_swapped},
			loop( ServerState#server_state{
					code_update_count =
						ServerState#server_state.code_update_count + 1,
					last_code_update = utils:get_timestamp() 
				},
				NewFunctionalModule, FunctionalState );

		{From,get_server_state} ->
			From ! {self(),state_to_string(ServerState)},
			loop( ServerState, FunctionalModule, FunctionalState );
	
		{From,Request} ->
			try FunctionalModule:handle( Request, FunctionalState ) of
			
				{Answer,NewFunctionalState} ->
					From ! {self(),Answer},
					loop( ServerState, FunctionalModule, NewFunctionalState )
			
			catch
			
				_:Why ->
					?emit_error([ io_lib:format(
						"Server whose state is ~w failed "
						"while executing request ~w for ~w: ~w.",
						[ServerState,Request,From,Why] ) ]),
						
					% Notify the client:	
					From ! {self(),request_failed,Why},
					
					% Transaction: loop with initial state (no change):
					loop( ServerState, FunctionalModule, FunctionalState )
					
			end				
	
	end.




% Returns a textual description of the specified server state.
state_to_string(ServerState) ->
	io_lib:format( "State of TCP server named ~s:~n"
		"  + started on ~s at ~s~n"
		"  + listening to TCP port ~w~n"
		"  + ~s~n"
		"  + current connections: ~w~n"
		"  + past connections: ~w~n",
		[ 
			ServerState#server_state.server_name,
			ServerState#server_state.host,
			utils:get_textual_timestamp(
				ServerState#server_state.starting_time), 
			ServerState#server_state.listening_port, 
			code_updates_to_string(ServerState),
			ServerState#server_state.current_connections,
			ServerState#server_state.past_connections
		] ).	



% Returns a textual description of past code updates.
code_updates_to_string(ServerState) ->

	case ServerState#server_state.last_code_update of 
		
		undefined ->
			"no functional code update performed yet" ;
			
		TimeStampPair ->
			io_lib:format( "~B functional code update(s) performed, "
				"last one was at ~s",
				[ 
					ServerState#server_state.code_update_count,
					utils:get_textual_timestamp(TimeStampPair) 
				] )
				
	end.
	

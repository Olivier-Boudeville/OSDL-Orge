% Custom-made TCP/IP server.
% Features:
%  - functional behaviour (client management) uncoupled from server
% implementation
%  - multiple parallel sessions can be handled
%  - hot code update
%  - simple state management
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
% For these TCP servers, there is one listening socket, and as many
% per-connection sockets as clients.
% TO-DO: limit the max number of simultaneous connections ?


% For server defaults and internal record:
-include("tcp_server.hrl").


% For emit_*:
-include("traces.hrl").




% Exported API section.

-export([create/3,create_link/3,send_request/2,send_request_by_name/2,
	update_code_for_client_management/2,state_to_string/1]).


% Creates a new TCP server.
%  - ServerName is the server name, under which it will be registered if
% requested
%  - RegistrationType is in 'none', 'local_only', 'global_only',
% 'local_and_global', depending on what kind of registration is requested 
% for this server
%  - ClientManagementModule is the Erlang module that will manage the 
% dialog with clients (each incoming connection will be given a dedicated
% spawned process running that code)
%
% Returns the PID of the launched server.
% (static)
create( ServerName, RegistrationType, ClientManagementModule ) ->

	?emit_trace([ io_lib:format( "Starting a new TCP server named ~s.",
		[ ServerName ] ) ]),
	
	% Avoids to export the init function:
	spawn( fun() -> init( ServerName,RegistrationType,ClientManagementModule)
		end ).
		

% Creates a new linked TCP server.
%  - ServerName is the server name, under which it will be registered if
% requested
%  - RegistrationType is in 'none', 'local_only', 'global_only',
% 'local_and_global', depending on what kind of registration is requested 
% for this server
%  - ClientManagementModule is the Erlang module that will manage the 
% dialog with clients (each incoming connection will be given a dedicated
% spawned process running that code)
%
% Returns the PID of the launched server.
% (static)
create_link( ServerName, RegistrationType, ClientManagementModule ) ->

	?emit_trace([ io_lib:format( "Starting a new linked TCP server named ~s.",
		[ ServerName ] ) ]),
			
	% Avoids to export the init function:
	spawn_link( fun() -> 
		init( ServerName,RegistrationType,ClientManagementModule) end ).



% Initializes the TCP server with specified name and module for client 
% management, and enters its main loop, listening for incoming connections.
% A default TCP port is used here.
init( ServerName, RegistrationType, ClientManagementModule ) ->
	init( ServerName, RegistrationType, ClientManagementModule, 
		?default_listening_tcp_server_port ).


% Initializes the TCP server with specified name, module for client 
% management and TCP port, and enters its main loop, listening for incoming
% connections.
init( ServerName, RegistrationType, ClientManagementModule, 
		ListeningTCPPort ) ->

	ok = utils:register_as( ServerName, RegistrationType ),

	?emit_debug([ io_lib:format( "Server will listen to TCP port ~B, "
		"with a maximum backlog of ~B.", 
		[ListeningTCPPort,?default_backlog] ) ]),
		
	% Listen to all network interfaces:
	{ok,ListenSocket} = gen_tcp:listen( ListeningTCPPort, [
		binary, {backlog,?default_backlog},	{reuseaddr,true} ] ),

	?emit_debug([ "Server waiting for incoming connections." ]),

	ServerState = #server_state{
		server_name = ServerName,
	    host = net_adm:localhost(),
	    starting_time = utils:get_timestamp(),
		current_client_module = ClientManagementModule,
	    client_code_update_count = 0,
	    last_client_code_update = undefined,
		listening_socket = ListenSocket,
	    listening_port = ListeningTCPPort,
		accepted_connections = [],
		closed_connections = []
	},
	loop( ServerState ).




% Client API.

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
update_code_for_client_management(ServerPid,NewClientManagementModule) ->
	send_request( ServerPid, {update_code_for_client_management,NewClientManagementModule} ).




% Internal API section.


% Main loop of this server.
%  - ServerState is the current state of this server (ex: containing the name
% under which the server is registered)
loop( ServerState ) ->

	?emit_debug([ io_lib:format( "~s~n", [ state_to_string(ServerState) ] ) ]),

	?emit_trace([ "Spawning a client manager for next connection." ]),

	% A pool of more than one manager waiting for accept could also be used:
	NewClientManagerPid = spawn( ?getState.current_client_module,
		init, [	self(), ?getState.listening_socket ] ),
	
	receive
	
		{NewClientManagerPid,accepted} ->
			loop( on_client_connection( ServerState, NewClientManagerPid ) );
					
		{AClientManagerPid,closed} ->
			loop( on_client_disconnection( ServerState, AClientManagerPid ) )
										
	end.



% Returns an updated server state.
on_client_connection( ServerState, ClientManagerPid ) ->

	?emit_debug([ io_lib:format( 
		"New connection: registering client manager ~w.",
		[ClientManagerPid] ) ]),
	
	?getState{ accepted_connections =
		[ ClientManagerPid | ?getState.accepted_connections ] }.


% Returns an updated server state.
on_client_disconnection( ServerState, ClientManagerPid ) ->
	CurrentConnections = ?getState.accepted_connections,
	case lists:member( ClientManagerPid, CurrentConnections ) of
	
		true ->
			?emit_debug([ io_lib:format( 
				"Closed connection: removing client manager ~w.",
				[ClientManagerPid] ) ]),
			?getState{ accepted_connections =
				lists:delete( ClientManagerPid, CurrentConnections ) };
				
		false ->
			?emit_error([ io_lib:format( "Error, closed connection ~w "
				"was not a registered one, nothing done.",
				[ClientManagerPid] ) ])
	
	end.
	




% Returns a textual description of the specified server state.
state_to_string(ServerState) ->
	io_lib:format( "State of TCP server named ~s:~n"
		"  + started on ~s at ~s~n"
		"  + listening to TCP port ~w~n"
		"  + ~s~n"
		"  + current client management module: ~w~n"
		"  + accepted (current) connections: ~w~n"
		"  + closed connections: ~w~n",
		[ 
			?getState.server_name,
			?getState.host,
			utils:get_textual_timestamp(
				?getState.starting_time), 
			?getState.listening_port, 
			?getState.current_client_module,
			code_updates_to_string(ServerState),
			?getState.accepted_connections,
			?getState.closed_connections
		] ).	



% Returns a textual description of past code updates.
code_updates_to_string(ServerState) ->

	case ?getState.last_client_code_update of 
		
		undefined ->
			"no update of client management code performed yet" ;
			
		TimeStampPair ->
			io_lib:format( "~B update(s) of client management code performed, "
				"last one was at ~s",
				[ 
					?getState.client_code_update_count,
					utils:get_textual_timestamp(TimeStampPair) 
				] )
				
	end.
	

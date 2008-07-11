% Orge TCP/IP server.
%
% Features:
%  - functional behaviour (client management) uncoupled from server
% implementation
%  - multiple parallel sessions can be handled
%  - hot code update
%  - simple state management
%
-module(orge_tcp_server).



% Implementation notes.

% Main objective is to uncouple the technical server from the functional one,
% so that one can be modified with no impact on the other.
%
% Not using OTP gen_server, to learn how to do it and, maybe, to improve 
% performances, ease of integration or adequation to the Orge specific needs
% (ex: contrary to the usual client/server approach, the Orge server has to
% "push", i.e. send to the clients, simulation updates without waiting for a
% specific request to do so).
%
% Inspired from the various servers described in 'Programming Erlang',
% by Joe Armstrong (chapter 16).
%
% WOOPER has not been used here to presumably speedup the processings, and 
% as no need for inheritance is expected.

% The role of these servers is simply to break incoming data stream into
% appropriate client requests passed to client managers, and to send updates
% to clients.
%
% For these TCP servers, there is one listening socket, and as many
% per-connection sockets as clients.
% TO-DO: limit the max number of simultaneous connections ?


% For server defaults, the internal record server_state and the getState macro:
-include("orge_tcp_server.hrl").


% For emit_*:
-include("traces.hrl").


% Shortcut macro, for convenience: 
-define(getState,ServerState#server_state).



% Exported API section.

-export([create/4,create_link/4,send_request/2,send_request_by_name/2,
	update_code_for_client_management/2,state_to_string/1]).


% Creates a new Orge TCP server.
%  - ServerName is the server name, under which it will be registered, if
% requested
%  - DatabaseInitializationMode tells whether it should be created empty
% (if from_scratch) or loaded from a previous instance (from_previous_state)
%  - RegistrationType is in 'none', 'local_only', 'global_only',
% 'local_and_global', depending on what kind of registration is requested 
% for this server
%  - ClientManagementModule is the Erlang module that will manage the 
% dialog with clients (each incoming connection will be given a dedicated
% spawned process running that code)
%
% Returns the PID of the launched server.
% (static)
create( ServerName, DatabaseInitializationMode, RegistrationType,
		ClientManagementModule ) ->

	?emit_trace([ io_lib:format( "Starting a new Orge TCP server named ~s.",
		[ ServerName ] ) ]),
	
	% Allows not to export the init function:
	spawn( fun() -> init( ServerName, DatabaseInitializationMode,
			RegistrationType, ClientManagementModule )
		end ).
		

% Creates a new linked Orge TCP server.
%  - ServerName is the server name, under which it will be registered if
% requested
%  - DatabaseInitializationMode tells whether it should be created empty
% (if from_scratch) or loaded from a previous instance (from_previous_state)
%  - RegistrationType is in 'none', 'local_only', 'global_only',
% 'local_and_global', depending on what kind of registration is requested 
% for this server
%  - ClientManagementModule is the Erlang module that will manage the 
% dialog with clients (each incoming connection will be given a dedicated
% spawned process running that code)
%
% Returns the PID of the launched server.
% (static)
create_link( ServerName, DatabaseInitializationMode, RegistrationType,
		ClientManagementModule ) ->

	?emit_trace([ io_lib:format( 
		"Starting a new linked Orge TCP server named ~s.", [ ServerName ] ) ]),
			
	% Avoids to export the init function:
	spawn_link( fun() -> init( ServerName, DatabaseInitializationMode,
			RegistrationType, ClientManagementModule )
		end ).



% Initializes the TCP server with specified name and module for client 
% management, and enters its main loop, listening for incoming connections.
% A default TCP listening port is used here.
init( ServerName, DatabaseInitializationMode, RegistrationType,
		ClientManagementModule ) ->
	init( ServerName, DatabaseInitializationMode, RegistrationType,
		ClientManagementModule, ?default_listening_orge_tcp_server_port ).


% Initializes the TCP server with specified name, module for client 
% management and TCP port, and enters its main loop, listening for incoming
% connections.
init( ServerName, DatabaseInitializationMode, RegistrationType,
		ClientManagementModule, ListeningTCPPort ) ->

	ok = utils:register_as( ServerName, RegistrationType ),

	?emit_debug([ io_lib:format( "Server will listen to TCP port ~B, "
		"with a maximum backlog of ~B.", 
		[ListeningTCPPort,?default_backlog] ) ]),
	DatabasePid = orge_database_manager:start_link(),
	% Listen to all network interfaces:
	% Packet and other informations are set for this listen socket, and these 
	% settings will be inherited by all accepted (server-side) sockets. 
	case gen_tcp:listen( ListeningTCPPort, [
			binary, {backlog,?default_backlog}, {active,true},
			{packet,?default_packet_header_size}, {reuseaddr,true} ] ) of 
			
		{ok,ListenSocket} ->
			% Only successful case:
			?emit_debug([ "Server waiting for incoming connections." ]),

			ServerState = #server_state{
				server_name = ServerName,
				host = net_adm:localhost(),
				%server_version = ?server_version,
				starting_time = utils:get_timestamp(),
				current_client_module = ClientManagementModule,
				client_code_update_count = 0,
				last_client_code_update = undefined,
				listening_socket = ListenSocket,
				listening_port = ListeningTCPPort,
				waiting_managers = [],
				accepted_connections = [],
				past_connections = []
				},
				% A pool of more than one manager waiting for accept could
				% also be used:
				NewState = create_manager( ServerState ),
				loop( NewState );
				
		{error,eaddrinuse} ->
				
			?emit_fatal([ io_lib:format( "Error, port ~s already in use "
				"(another Orge server instance already running?). "
				"Stopping this server.",
				[utils:integer_to_string(ListeningTCPPort) ] ) ]);

		{error,OtherError} ->
				
			?emit_fatal([ io_lib:format( "Error, server could not be launched "
				"(reason: ~s), stopping this server.", [OtherError] ) ])

	end.




% Client API.


% Sends specified request to specified PID or name registered locally.
% (static)
send_request( ServerPid, Request ) when is_pid(ServerPid) ->
	ServerPid ! {self(),Request},
	receive
	
		{ServerPid,Answer} ->
			Answer;

		{ServerPid,request_failed,Message} ->
			{request_failed,Message}		
	
	end.


% (static)
send_request_by_name( ServerName, Request ) ->
	send_request( erlang:whereis(ServerName), Request ).
	

% Replaces, for specified server, current version of functional code by
% specified one.
update_code_for_client_management(ServerPid,NewClientManagementModule) ->
	send_request( ServerPid,
		{update_code_for_client_management,NewClientManagementModule} ).




% Internal API section.

		

% Creates a client manager waiting for any newly accepted connection.
% Returns an updated state.
create_manager( ServerState ) ->

	?emit_trace([ "Spawning a client manager for next connection." ]),

	NewClientManagerPid = spawn_link( ?getState.current_client_module,
		init, [	self(), ?getState.listening_socket ] ),
		
	?getState{ waiting_managers =
		[ NewClientManagerPid | ?getState.waiting_managers ] }.



% Main loop of this TCP server.
% ServerState is the current state of this server (ex: containing the name
% under which the server is registered and all other informations).
loop( ServerState ) ->
	
	?emit_trace([ "Orge server waiting for messages." ]),
	
	?emit_debug([ io_lib:format( "~s~n", [ state_to_string(ServerState) ] ) ]),
	
	receive
	
		{NewClientManagerPid,accepted} ->
			loop( on_client_connection( ServerState, NewClientManagerPid ) );
					
		{AClientManagerPid,closed,Login,Times} ->
			loop( on_client_disconnection( ServerState, AClientManagerPid,
				Login,Times ) );
		
		{SenderPid,get_info} ->
			SenderPid ! {server_info,state_to_string(ServerState)},
			loop( ServerState );
									
		{SenderPid,shutdown} ->
			on_shutdown_request( ServerState, SenderPid )
			% No loop( ServerState ) here !
									
	end.



% Returns an updated server state.
on_client_connection( ServerState, ClientManagerPid ) ->

	?emit_debug([ io_lib:format( 
		"New connection: registering client manager ~w.",
		[ClientManagerPid] ) ]),
	
	NewState = ?getState{ waiting_managers = lists:delete( ClientManagerPid,
		?getState.waiting_managers ) },
	
	% No test for multiple registering of a manager necessary here:
	ManagerLessState = NewState#server_state{ accepted_connections =
		[ ClientManagerPid | ?getState.accepted_connections ] },
	
	create_manager(ManagerLessState).


% Returns an updated server state.
on_client_disconnection( ServerState, ClientManagerPid, Login, 
		{StartingTime,StoppingTime} ) ->
	CurrentConnections = ?getState.accepted_connections,
	case lists:member( ClientManagerPid, CurrentConnections ) of
	
		true ->
			?emit_debug([ io_lib:format( "Closed connection: "
				"removing client manager ~w corresponding to login ~s.",
				[ClientManagerPid,Login] ) ]),
			ClientStats = {Login,StartingTime,StoppingTime},	
			?getState{ 
				accepted_connections =
					lists:delete( ClientManagerPid, CurrentConnections ),
				past_connections = [ClientStats|?getState.past_connections]	
				 };
				
		false ->
			?emit_error([ io_lib:format( "Error, closed connection ~w "
				"was not a registered one, nothing done.",
				[ClientManagerPid] ) ])
	
	end.
	
% User creation: make_ref()


% Manages a server shutdown.
on_shutdown_request( ServerState, SenderPid ) ->
	?emit_trace([ io_lib:format( 
		"Received a shutdown request from ~w, notifying client managers.",
		[SenderPid] ) ]),
	lists:foreach( fun(Manager) -> Manager ! orge_server_shutdown end, 
		?getState.accepted_connections ),
	?emit_info([ "Shutdown completed." ]).



% Returns a textual description of the specified server state.
state_to_string(ServerState) ->
	io_lib:format( "State of TCP Orge server version ~.1f named ~s:~n"
		"  + started on ~s at ~s~n"
		"  + listening to TCP port ~w~n"
		"  + current client management module: ~s~n"
		"  + ~s~n"
		"  + ~s~n"
		"  + ~s~n"
		"  + ~s~n",
		[ 
			?getState.server_version,
			?getState.server_name,
			?getState.host,
			utils:get_textual_timestamp(?getState.starting_time), 
			?getState.listening_port, 
			?getState.current_client_module,
			code_updates_to_string(ServerState),
			waiting_managers_to_string( ?getState.waiting_managers ),
			accepted_connections_to_string( ?getState.accepted_connections ),
			past_connections_to_string( ?getState.past_connections )
		] ).	



% Returns a textual description of past code updates.
code_updates_to_string(ServerState) ->

	case ?getState.last_client_code_update of 
		
		undefined ->
			"no update of client management code performed yet" ;
			
		TimeStampPair ->
			io_lib:format( "~B update(s) of client management code performed, "
				"last one was at ~s", [ 
					?getState.client_code_update_count,
					utils:get_textual_timestamp(TimeStampPair) 
				] )
				
	end.




% Returns a textual description of current accepted connections.

waiting_managers_to_string( [] ) -> 
	"no waiting manager available" ;

waiting_managers_to_string( ManagerList ) -> 
	io_lib:format( "~s manager(s) waiting", 
		[ utils:integer_to_string( length(ManagerList) )] ).




% Returns a textual description of current accepted connections.

accepted_connections_to_string( [] ) -> 
	"no connection accepted currently" ;

accepted_connections_to_string( AcceptedConnections ) -> 
	accepted_connections_to_string( AcceptedConnections, [] ).


accepted_connections_to_string( [], Acc ) ->
	 io_lib:format( "currently accepted connections:~n", [] ) 
		++ lists:flatten( Acc ) ;
	
accepted_connections_to_string( [H|T], Acc ) ->
	accepted_connections_to_string( T, 
		[ io_lib:format( "      * ~w~n", [H] ) |�Acc ] ).
	

	
% Returns a textual description of past connections.

past_connections_to_string( [] ) ->
	"no past connection yet" ;

past_connections_to_string( PastConnections ) ->
	past_connections_to_string( PastConnections, [] ).
	
	
past_connections_to_string( [], Acc ) ->
	 io_lib:format( "past connections:~n", [] ) 
		++ lists:flatten( Acc ) ;
	
past_connections_to_string( [{Login,StartingTime,StoppingTime}|T], Acc ) ->
	past_connections_to_string( T, 
		[ io_lib:format( "   * ~s~n", 
				[ past_connection_to_string(Login,StartingTime,StoppingTime) ] )
			|�Acc ] ).
	

past_connection_to_string( not_logged_in, StartingTime, StoppingTime ) ->
	io_lib:format( "connection stopped before user log-in, "
		"from ~s to ~s, duration: ~s", 
		[ 	utils:get_textual_timestamp(StartingTime),
			utils:get_textual_timestamp(StoppingTime),
			utils:get_textual_duration(StartingTime,StoppingTime) ] );

past_connection_to_string( Login, StartingTime, StoppingTime ) ->
	io_lib:format( "connection of logged user '~s', "
		"from ~s to ~s, duration: ~s", 
		[ Login,
			utils:get_textual_timestamp(StartingTime),
			utils:get_textual_timestamp(StoppingTime),
			utils:get_textual_duration(StartingTime,StoppingTime) ] ).
		
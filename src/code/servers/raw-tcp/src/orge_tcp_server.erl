% 
% Copyright (C) 2003-2009 Olivier Boudeville
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


% Orge TCP/IP server.
%
% Features:
%  - functional behaviour (client management) uncoupled from server
% implementation
%  - multiple parallel sessions can be handled
%  - hot code update
%  - simple state management
%  - database integration
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
% Servers are not expected to record informations about connections: this is
% done by the Orge database.
%
% Servers just keep track of the client managers they launch, as they control
% their lifecycle.
%
% Currently running managers are split between waiting ones and the ones having
% accepted their connection.
% 
% Servers count as well the total number of spawned managers, to preserve unique
% identifier (thanks to a manager counter) despite terminations of managers.
%
% For these TCP servers, there is one listening socket, and as many
% per-connection sockets as clients.
%
% TO-DO: limit the maximum number of simultaneous connections ?
%


% For server defaults, the internal record server_state and the getState macro:
-include("orge_tcp_server.hrl").


% For emit_*:
-include("traces.hrl").


% Shortcut macro, for convenience: 
-define(getState,ServerState#server_state).



% Exported API section.

-export([create/3,create_link/3,create/4,create_link/4,state_to_string/1]).


% The version of this TCP server.
-define(server_version,0.1).


% Internal record that stores the current state of a TCP server:
-record( server_state, {
	server_name,
	host,
	server_version = ?server_version,
	starting_time,
	database_pid,
	current_client_module,
	client_code_update_count = 0,
	last_client_code_update,
	listening_socket,
	listening_port,
	waiting_managers = [],
	accepted_connections = [],
	connection_count = 0
} ).


% Creates a new Orge TCP server.
%  - ServerName is the server name, under which it will be registered, if
% requested
%  - DatabaseInitializationMode tells whether it should be created empty
% (if from_scratch) or loaded from a previous instance (from_previous_state)
%  - RegistrationType is in 'none', 'local_only', 'global_only',
% 'local_and_global', depending on what kind of registration is requested 
% for this server
%
% The default client management module will be used.
%
% Returns the PID of the launched server.
% (static)
create( ServerName, DatabaseInitializationMode, RegistrationType ) ->

	?emit_trace([ io_lib:format( "Starting a new Orge TCP server named ~s.",
		[ ServerName ] ) ]),
	
	% Allows not to export the init function:
	spawn( fun() -> init( ServerName, DatabaseInitializationMode,
			RegistrationType )
		end ).
		

% Creates a new linked Orge TCP server.
%  - ServerName is the server name, under which it will be registered if
% requested
%  - DatabaseInitializationMode tells whether it should be created empty
% (if from_scratch) or loaded from a previous instance (from_previous_state)
%  - RegistrationType is in 'none', 'local_only', 'global_only',
% 'local_and_global', depending on what kind of registration is requested 
% for this server
%
% The default client management module will be used.
%
% Returns the PID of the launched server.
% (static)
create_link( ServerName, DatabaseInitializationMode, RegistrationType ) ->

	?emit_trace([ io_lib:format( 
		"Starting a new linked Orge TCP server named ~s.", [ ServerName ] ) ]),
			
	% Avoids to export the init function:
	spawn_link( fun() -> init( ServerName, DatabaseInitializationMode,
			RegistrationType )
		end ).




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



% Initializes the TCP server with specified name, and enters its main loop,
% listening for incoming connections.
% Default client management module and TCP listening port are used here.
init( ServerName, DatabaseInitializationMode, RegistrationType ) ->
	init( ServerName, DatabaseInitializationMode, RegistrationType,
		?default_client_management_module,
		?default_listening_orge_tcp_server_port ).


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

	?emit_trace([ io_lib:format( "Orge server will listen to TCP port ~B, "
		"with a maximum backlog of ~B.", 
		[ListeningTCPPort,?default_backlog] ) ]),
		
	?emit_trace([ io_lib:format( "Starting the Orge database, in '~w' mode.",
		[DatabaseInitializationMode] ) ]),
		
	DatabasePid = orge_database_manager:start_link( 
		DatabaseInitializationMode, self() ),	
	receive
	
		orge_database_ready ->
			?emit_trace([ "Orge database ready." ])
			
	end,	
	
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
				database_pid = DatabasePid,
				current_client_module = ClientManagementModule,
				client_code_update_count = 0,
				last_client_code_update = undefined,
				listening_socket = ListenSocket,
				listening_port = ListeningTCPPort,
				waiting_managers = [],
				accepted_connections = [],
				connection_count = 0
				},
			% A pool of more than one manager waiting for accept could
			% also be used:
			NewState = create_client_manager( ServerState ),
			loop( NewState );
				
		{error,eaddrinuse} ->
				
			?emit_fatal([ io_lib:format( "Error, port ~s already in use "
				"(another Orge server instance already running?). "
				"Stopping this server.",
				[utils:integer_to_string(ListeningTCPPort) ] ) ]);

		{error,OtherError} ->
				
			?emit_fatal([ io_lib:format( "Error, server could not be launched "
				"(reason: ~w), stopping this server.", [OtherError] ) ])

	end.





% Internal API section.

		

% Creates a client manager waiting for any newly accepted connection.
% Returns an updated state.
create_client_manager( ServerState ) ->

	?emit_trace([ "Spawning a client manager for next future connection." ]),

	NewConnectionCount = ?getState.connection_count + 1,

	NewClientManagerPid = spawn_link( ?getState.current_client_module,
		init, [	self(), ?getState.listening_socket, NewConnectionCount,
			?getState.database_pid ] ),
	
	% Waiting managerS as a pool of pre-spawn managers could be used:	
	?getState{ 
		waiting_managers = [ NewClientManagerPid | ?getState.waiting_managers ],
		connection_count = NewConnectionCount
	}.



% Main loop of this TCP server.
% ServerState is the current state of this server (ex: containing the name
% under which the server is registered and all other informations).
loop( ServerState ) ->
	
	?emit_trace([ "Orge server waiting for messages." ]),
	
	?emit_debug([ io_lib:format( "~s~n", [ state_to_string(ServerState) ] ) ]),
	
	receive
	
		{register_user,NewUserSettings,CallerPid} ->
			{NewState,Result} = on_user_registration( ServerState, 
				NewUserSettings ),
			CallerPid ! {registration_result,Result},
			loop(NewState);
			 			
		{unregister_user,UserLogin,CallerPid} ->
			{NewState,Result} = on_user_unregistration( ServerState, 
				UserLogin ),
			CallerPid ! {unregistration_result,Result},
			loop(NewState);
			 			
		{NewClientManagerPid,accepted} ->
			loop( on_client_connection( ServerState, NewClientManagerPid ) );
					
		{AClientManagerPid,closed} ->
			loop( on_client_disconnection( ServerState, AClientManagerPid ) );
		
		{SenderPid,get_info} ->
			SenderPid ! {server_info,state_to_string(ServerState)},
			loop( ServerState );
									
		{SenderPid,shutdown} ->
			on_shutdown_request( ServerState, SenderPid )
			% No loop( ServerState ) here !
	end.


% Declares a new Orge user for future logins.
% Returns {NewState,Result}. 
on_user_registration( ServerState, NewUserSettings ) ->
	?getState.database_pid ! {register_user,NewUserSettings,self()},
	receive
	
		RegistrationResult ->
			{ServerState,RegistrationResult}
	
	end.
	
			 			
% Unregisters an Orge user, specified by login.
% Returns {NewState,Result}. 
on_user_unregistration( ServerState, UserLogin ) ->
	?getState.database_pid ! {unregister_user,UserLogin,self()},
	receive
	
		UnregistrationResult ->
			{ServerState,UnregistrationResult}
	
	end.



% Returns an updated server state.
on_client_connection( ServerState, ClientManagerPid ) ->

	?emit_debug([ io_lib:format( 
		"New connection: registering client manager ~w.",
		[ClientManagerPid] ) ]),
	
	NewState = ?getState{ waiting_managers = lists:delete( ClientManagerPid,
		?getState.waiting_managers ) },
	
	% No test for multiple registering of a manager necessary here:
	ManagerLessState = NewState#server_state{ 
		accepted_connections =
			[ ClientManagerPid | ?getState.accepted_connections ]
	},
	create_client_manager(ManagerLessState).


% Returns an updated server state.
on_client_disconnection( ServerState, ClientManagerPid ) ->
	CurrentConnections = ?getState.accepted_connections,
	case lists:member( ClientManagerPid, CurrentConnections ) of
	
		true ->
			?emit_debug([ io_lib:format( "Closed connection: "
				"removing client manager ~w.",[ClientManagerPid] ) ]),
			?getState{ accepted_connections =
				lists:delete( ClientManagerPid, CurrentConnections ) };
				
		false ->
			?emit_error([ io_lib:format( "Error, closed connection ~w "
				"was not a registered one, nothing done.",
				[ClientManagerPid] ) ]),
			ServerState	
	
	end.
	


% Manages a server shutdown.
on_shutdown_request( ServerState, SenderPid ) ->

	?emit_trace([ io_lib:format( 
		"Received a shutdown request from ~w, notifying client managers.",
		[SenderPid] ) ]),
	% Waiting managers will be notified by the closing of the listening socket.	
	lists:foreach( 
		fun(Manager) -> 
			unlink(Manager),
			Manager ! orge_server_shutdown
		end, 
		?getState.accepted_connections ),
		
	?emit_trace([ "Shutting down database." ]),
	?getState.database_pid ! {stop,self()},
	receive
	
		orge_database_stopped ->
			ok
			
	end,
	?emit_info([ "Shutdown completed." ]),
	SenderPid ! orge_server_shutdown.



% Returns a textual description of the specified server state.
state_to_string(ServerState) ->
	io_lib:format( "State of TCP Orge server version ~.1f named ~s:~n"
		"  + started on ~s at ~s~n"
		"  + using database instance ~w~n"
		"  + listening to TCP port ~w~n"
		"  + current client management module: ~s~n"
		"  + ~s~n"
		"  + ~s~n"
		"  + ~s~n",
		[ 
			?getState.server_version,
			?getState.server_name,
			?getState.host,
			utils:get_textual_timestamp(?getState.starting_time), 
			?getState.database_pid, 
			?getState.listening_port, 
			?getState.current_client_module,
			code_updates_to_string(ServerState),
			waiting_managers_to_string( ?getState.waiting_managers ),
			accepted_connections_to_string( ?getState.accepted_connections )
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
	 io_lib:format( "currently accepted connections:", [] ) 
		++ lists:flatten( Acc ) ;
	
accepted_connections_to_string( [H|T], Acc ) ->
	accepted_connections_to_string( T, 
		[ io_lib:format( "~n      * ~w", [H] ) | Acc ] ).
	

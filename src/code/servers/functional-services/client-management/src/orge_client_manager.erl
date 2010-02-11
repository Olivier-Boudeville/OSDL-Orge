% Copyright (C) 2003-2010 Olivier Boudeville
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


% Server-side module dedicated to the management of a client connection.
% Features:
%  - functional behaviour (client management) uncoupled from server
% implementation
%  - transaction semantics
%  - database integration
%
-module(orge_client_manager).


% Implementation notes.


% TO-DO:
%
%  - the client manager could buffer a bit sendings, lest gen_tcp:send
% eats too much CPU?
%


-export([ init/4 ]).


% For manager_state:
-include("orge_client_manager.hrl").


% For default_identifier_separator:
-include("orge_tcp_server.hrl").



% The version of this client manager:
-define(manager_version,{0,1,0}).


% The oldest client version that this client manager supports:
-define(oldest_supported_client_version,?manager_version).



% Internal record that stores the current state of the server-side manager of 
% a client connection:
-record( manager_state, {

	% Stores any login used by a client:
	client_login,
	
	% Address, port and DNS name for the peer (connected client), i.e.
	% {IPv4Address,Port,DNSLookup}:
	client_host,

	% Version of this manager:
	manager_version,
	
	% Oldest supported client version:
	oldest_supported_client_version,
	
	% Timestamp to record when this manager started:
	starting_time,
	
	% The client socket returned by accept:
	client_socket,
	
	% The version reported by the client:
	client_version,
	
	% The Pid of the Orge server that spawned this manager:
	server_pid,
	
	% The identifier of that connection (actually its number):
	connection_identifier,
	
	% The Pid of the Orge database:
	database_pid 
	        
} ).



% Shortcut macro, for convenience: 
-define(getState,ManagerState#manager_state).



		


% Trace section.

% Note: can be factorized in a header or in a module due to the
% use of the defines. Use of macros implies to define them first. 


% For emit_*:
-include("traces.hrl").


-define( trace_emitter_categorization, "Orge" ).


get_emitter_name( ManagerState ) ->
	forge_emitter_name( ?getState.connection_identifier ).
	
	
forge_emitter_name( ConnectionID ) ->
	io_lib:format( "Client Manager #~B", [ ConnectionID ] ).
	 


	
%send_fatal( Message, ManagerState ) ->
%	?notify_fatal( Message, get_emitter_name( ManagerState ), 
%		?trace_emitter_categorization ).

%-define( fatal( Message ), send_fatal( Message, ManagerState ) ).

	
	
send_error( Message, ManagerState ) ->
	?notify_error( Message, get_emitter_name( ManagerState ), 
		?trace_emitter_categorization ).
	
-define( error( Message ), send_error( Message, ManagerState ) ).


	
send_warning( Message, ManagerState ) ->
	?notify_warning( Message, get_emitter_name( ManagerState ), 
		?trace_emitter_categorization ).
	
-define( warning( Message ), send_warning( Message, ManagerState ) ).

send_warning_stateless( Message, ConnectionID ) ->
	?notify_warning( Message, forge_emitter_name( ConnectionID ), 
		?trace_emitter_categorization ).
	

	
send_info( Message, ManagerState ) ->
	?notify_info( Message, get_emitter_name( ManagerState ), 
		?trace_emitter_categorization ).
	
-define( info( Message ), send_info( Message, ManagerState ) ).
	

	
send_trace( Message, ManagerState ) ->
	?notify_trace( Message, get_emitter_name( ManagerState ), 
		?trace_emitter_categorization ).

%send_trace_stateless( Message, ConnectionID ) ->
%	?notify_trace( Message, forge_emitter_name( ConnectionID ), 
%		?trace_emitter_categorization ).
	
-define( trace( Message ), send_trace( Message, ManagerState ) ).
	

	
send_debug( Message, ManagerState ) ->
	?notify_debug( Message, get_emitter_name( ManagerState ), 
		?trace_emitter_categorization ).
	
-define( debug( Message ), send_debug( Message, ManagerState ) ).


send_debug_stateless( Message, ConnectionID ) ->
	?notify_debug( Message, forge_emitter_name( ConnectionID ), 
		?trace_emitter_categorization ).
	




% Inits the client socket for that client manager.
init( ServerPid, ListenSocket, ConnectionIdentifier, DatabasePid ) ->

	%io:format( "Client manager ~w created.~n", [self()] ),
	
	% Not state yet:
	send_debug_stateless( 
		io_lib:format( "Client manager ~w waiting for next connection #~B.", 
			[self(),ConnectionIdentifier] ), ConnectionIdentifier ),
			
	% This process will be the one controlling the client socket once created;
	% accept will block until a connection is made:	
	case gen_tcp:accept(ListenSocket) of
	
		{ok,ClientSocket} ->

			%io:format( "Client manager ~w accepted connection.~n", [self()] ),
			
			% Configures as soon as possible this new socket
			% (risk of race condition?):
			inet:setopts( ClientSocket, [
				
				% Received Packet is delivered as a binary:
				binary, 

				% Using 'once' allows for flow control (a fast sender must not
				% be able to easily overflow this manager):	
				{active,once},

				% A constant size header will specify the number of bytes in the
				% packets:
				{packet,?default_packet_header_size},
				
				% Allows the local reuse of the port number:
				{reuseaddr,true},
				
				% Enables periodic transmission on the socket, to avoid the
				% connection is considered broken if idle too long:
				{keepalive,true}
				
			]),

			% Allows the server to handle next connections ASAP:
			ServerPid ! {self(),accepted},

			{ok, {ClientAddress,ClientPort} } = inet:peername(ClientSocket),
		
			ReversedDNS = net_utils:reverse_lookup(ClientAddress), 
						
			
			ManagerState = #manager_state{
				client_login = not_tried_yet,
				client_host = {ClientAddress,ClientPort,ReversedDNS},
				manager_version = ?manager_version,
				oldest_supported_client_version =
					?oldest_supported_client_version,
			    starting_time = basic_utils:get_timestamp(),
				client_socket = ClientSocket,
				server_pid = ServerPid,
				connection_identifier = ConnectionIdentifier,
				database_pid = DatabasePid
			},

			?debug( io_lib:format( "Connection ~B accepted.",
				[ConnectionIdentifier] ) ),
			
			% Useful to introduce random delays:
			% (seeding necessary otherwise always the same values)
			{A1,A2,A3} = now(),
			random:seed( A1, A2, A3 ),
			
			% Ensures that we do not exceed the maximum connection count:
			case check_connection_allowed( ManagerState ) of
			
				slot_obtained ->
					?debug( "A slot could be obtained for this connection." ), 
					control_access( ManagerState );
					
				slot_refused ->
					?warning( "No slot could be obtained for this connection, "
						"notifying the client and terminating." ),
						
					random_wait(), 
					gen_tcp:send( ClientSocket,	<<?no_slot_available>> ),
					terminate_manager(ManagerState)
						
			end;
			
		{error,closed} ->
			% No state yet:
			send_warning_stateless( "Unable to accept a connection on "
				"listening socket, server must have shut it down, "
				"stopping this client manager now.", ConnectionIdentifier )
	
	end.			
	

	
% Ensures that we do not exceed the maximum count of active connections, by
% requesting the database.
check_connection_allowed( ManagerState ) ->

	ConnectionId = ?getState.connection_identifier,

	?debug( io_lib:format( "Connection ~B requesting a server slot.", 
		[ ConnectionId ] ) ),
		
	?getState.database_pid	! { request_slot, ConnectionId, 
		?getState.client_host, self() },
		
	receive
	
		{ConnectionId,SlotAnswer} ->
			SlotAnswer
	end.
	
	
		
% Checks that the associated client is known from the server.
% Protocol:
%   - client sends a packet containing two sequences of bytes separated by
% ?default_identifier_separator
%   - server interprets the first sequence as a login, the second as a password
%   - if the incoming packet is ill-formatted, or in the login/password pair is 
% unknown, or if the account is already in use, or if a time-out occurs, 
% appropriate errors are traced
control_access( ManagerState ) ->

	ClientSocket = ?getState.client_socket,	
	net_utils:activate_socket_once( ClientSocket ),

	receive
		
		{tcp,ClientSocket,Data} ->
			% Thanks to initial header, we received here a full packet.
			% First message from client must be login informations.
			%?debug( io_lib:format( 
			%	"Client manager received following TCP message: ~w.", 
			%	[Data] ) ),
			Id = binary_to_list(Data),
			Identifiers = string:tokens( Id, ?default_identifier_separator ),
			% Precise reasons of failures not reported to the client to avoid
			% too straightforward attacks:
			case Identifiers of 
			
				[Login,Password] ->
					
					case declare_identifiers( ManagerState, Login, Password ) of
						
						access_granted ->
							?trace( io_lib:format( 
								"Connection acccepted for login '~s' and "
								"password '~s'.", [Login,Password] ) ),
							gen_tcp:send( ClientSocket,	<<?access_granted>> ),
							check_client_version( 
								?getState{ client_login = access_granted } );
							
						bad_login ->
							?warning( io_lib:format( 
								"Connection refused for login '~s' and "
								"password '~s': bad (unregistered) login.",
								 [Login,Password] ) ),
							gen_tcp:send( ClientSocket,	<<?access_denied>> ),
							terminate_manager(ManagerState);
							
						bad_password ->
							?warning( io_lib:format( 
								"Connection refused for login '~s' and "
								"password '~s': registered login, but "
								"incorrect password.",
								 [Login,Password] ) ),
							gen_tcp:send( ClientSocket,	<<?access_denied>> ),
							terminate_manager(ManagerState);
							
						already_connected ->
							?warning( io_lib:format( 
								"Connection refused for login '~s' and "
								"password '~s': correct login and password, "
								"but account already in use.",
								 [Login,Password] ) ),
							gen_tcp:send( ClientSocket,	<<?already_connected>>),
							terminate_manager(ManagerState);
														
						account_not_active ->
							?warning( io_lib:format( 
								"Connection refused for login '~s' and "
								"password '~s': correct login and password, "
								"but account not active.",
								 [Login,Password] ) ),
							gen_tcp:send( ClientSocket,	<<?access_denied>>),
							terminate_manager(ManagerState);
				
						internal_error ->
							?warning( io_lib:format( 
								"Connection refused for login '~s' and "
								"password '~s': internal error.",
								 [Login,Password] ) ),
							gen_tcp:send( ClientSocket,	<<?access_denied>>),
							terminate_manager(ManagerState)

					end;
					
				_IdentifierList ->
					% Parsing did not result in exactly two elements:
					?error( io_lib:format( 
						"Error, unable to determine login identifiers "
						"in '~w', connection refused.", [Identifiers] ) ),
						
					?getState.database_pid ! { declare_marshalling_failed,
						?getState.connection_identifier },	
					
					random_wait(),
							
					gen_tcp:send( ClientSocket, <<?ill_formatted_identifiers>>),
					terminate_manager(ManagerState)
					
			end;		
			
			
		Any ->
		
			?debug( io_lib:format( "Client manager received "
				"unhandled message while not identified: ~w (ignored).",
				[Any] ) ),								
			control_access( ManagerState )
	
	
	% Milliseconds:
	after 5000 ->
	
		?warning( "Time-out while waiting for the client identification, "
			"notifying the client and the server, and "
			"stopping this client manager."
			),
			
		?getState.database_pid	! { declare_timed_out, 
			?getState.connection_identifier },
			
		gen_tcp:send( ?getState.client_socket, <<?timed_out>> ), 
		terminate_manager(ManagerState)
				
	end.


% Checks that the client has a relevant version.
check_client_version( ManagerState ) ->

	ClientSocket = ?getState.client_socket,	
	net_utils:activate_socket_once( ClientSocket ),

	receive
	
		{tcp,ClientSocket,Data} ->
			ClientVersion = list_to_tuple( binary_to_list( Data) ),
			case basic_utils:compare_versions( ?getState.manager_version,
					ClientVersion ) of
				
				first_bigger ->
					?error( io_lib:format( "Client version (~s) too old, "
						"ending connection ~B.", [
							basic_utils:version_to_string(ClientVersion),
							?getState.connection_identifier ] ) ),
						
					?getState.database_pid	! { declare_incompatible_version,
						ClientVersion },
			
					gen_tcp:send( ClientSocket,
						<<?incompatible_version>> ),
					
					terminate_manager(ManagerState);
						
					
				_EqualOrSecondBigger ->
				
					?trace( io_lib:format( "Client version ~s acccepted.",
						[basic_utils:version_to_string(ClientVersion)] ) ),
						
					gen_tcp:send( ClientSocket,	<<?compatible_version>> ),

					loop( ?getState{ client_version = ClientVersion } )
		 
			end
			
	end.



% Main loop of this client manager, once the client has been identified.
% ManagerState is the current state of this manager (ex: containing the name
% under which the server is registered and all other informations).
loop( ManagerState ) ->
	
	ClientSocket = ?getState.client_socket,	
	net_utils:activate_socket_once( ClientSocket ),

	% Expected to end with: declare_shutdown_by_server
	
	
	receive
	
		orge_server_shutdown ->
			on_server_shutdown( ManagerState );		
			% No loop here.
		
		{tcp_closed,ClientSocket} ->
			?debug( "Client connection closed, "
				"notifying database and server." ),
				
			?getState.database_pid	! { declare_unexpected_client_termination, 
				?getState.connection_identifier },
				
			?getState.server_pid ! {self(),closed};
			% No loop here.
			
		{tcp_closed,Other} ->
			?debug( io_lib:format(  
				"Unexpected closing of client connection received "
				"(from ~w instead of from ~w), ignored.", 
				[Other,ClientSocket] ) ),
			loop( ManagerState );	
			

		{tcp,ClientSocket,<<?normal_client_side_termination>>} ->
			?info( "Received a notification of client-side termination." ),
			?getState.database_pid	! { declare_normal_client_side_termination, 
				?getState.connection_identifier },
			terminate_manager(ManagerState);
		
		{tcp,ClientSocket,Data} ->
			io:format( "Received unhandled data: ~p.", [Data] );
				
		Any ->
			?debug( io_lib:format( "Client manager received "
				"unhandled message while identified: ~w (ignored).",
				[Any] ) ),
			loop( ManagerState )
	
	end.



% To be called whenever this client manager determined it should stop.
terminate_manager(ManagerState) ->
	gen_tcp:close( ?getState.client_socket ),
	?getState.server_pid ! {self(),closed},
	?info( "Client manager terminated." ).
	% Nothing more here, terminating.	


	
on_server_shutdown(ManagerState) ->
	?info( "Received a shutdown notification from server, "
		"notifying client and stopping now." ),
	?getState.database_pid	! { declare_shutdown_by_server,
		?getState.connection_identifier },
	gen_tcp:send( ?getState.client_socket, <<?shutdown_notification>> ),
	terminate_manager(ManagerState).
	
	
	
% Checks that specified login/password are correct,and not already in use.
% Registers in all cases the connection in database.	
declare_identifiers( ManagerState, Login, Password ) ->
	ConnectionId = ?getState.connection_identifier,
	{ClientIP,ClientPort,ReversedDNS} = ?getState.client_host,
	DNSString = case ReversedDNS of
	
		unknown_dns ->
			"(DNS could not be resolved)";
		
		DNSName ->
			"whose DNS name is " ++ DNSName
			
	end,

	?debug( io_lib:format( 
		"Received login '~s' and password '~s', checking database "
		"from connection ~B for ~s ~s.", 
		[ Login, Password, ConnectionId,
			basic_utils:ipv4_to_string(ClientIP,ClientPort), DNSString ] ) ),
		
	?getState.database_pid	! { try_login, Login, Password, ConnectionId,
		self() },
		
	receive
	
		{ConnectionId,Answer} ->
			Answer
			
	end.

	
	
% Waits for a random duration between 1 and 2 seconds.
% May be used to fight denial of service and guessings based on request
% durations.
random_wait() ->
	timer:sleep( 1000 + random:uniform(1000) ).
		

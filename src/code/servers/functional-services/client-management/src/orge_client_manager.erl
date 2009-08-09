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
%  - client and server should check that their versions are compatible
%


-export([ init/4 ]).


% For manager_state:
-include("orge_client_manager.hrl").


% For default_identifier_separator:
-include("orge_tcp_server.hrl").


% For emit_*:
-include("traces.hrl").



% The version of this client manager:
-define(manager_version,0.1).


% The oldest client version that this client manager supports:
-define(oldest_supported_client_version,?manager_version).



% Internal record that stores the current state of the server-side manager of 
% a client connection:
-record( manager_state, {

	% Stores any login used by a client:
	client_login,
	
	% Address and port for the connected client ({IPv4,Port}):
	client_host,

	% Version of this manager:
	manager_version,
	
	% Oldest supported client version:
	oldest_supported_client_version,
	
	% Timestamp to record when this manager started:
	starting_time,
	
	% The client socket returned by accept:
	client_socket,
	
	% The Pid of the Orge server that spawned this manager:
	server_pid,
	
	% The identifier of that connection (actually its number):
	connection_identifier,
	
	% The Pid of the Orge database:
	database_pid 
	        
} ).



% Shortcut macro, for convenience: 
-define(getState,ManagerState#manager_state).



% Inits the client socket for that client manager.
init( ServerPid, ListenSocket, ConnectionIdentifier, DatabasePid ) ->

	?emit_debug([ io_lib:format( 
		"Client manager ~w waiting for next connection #~B.", 
			[self(),ConnectionIdentifier] ) ]),
			
	% This process will be the one controlling the client socket once created;
	% accept will block until a connection is made:	
	case gen_tcp:accept(ListenSocket) of
	
		{ok,ClientSocket} ->
		
			% Allows the server to handle next connections ASAP:
			ServerPid ! {self(),accepted},
			
			?emit_debug([ io_lib:format( "Connection ~B accepted.",
				[ConnectionIdentifier] ) ]),
			
			inet:setopts( ClientSocket, [
				
				% Received Packet is delivered as a binary:
				binary, 

				% FIXME Using 'once' allows for flow control:	
				%{active,once},
				{active,true},

				% A constant size header will specify the number of bytes in the
				% packets:
				{packet,?default_packet_header_size},
				
				% Allows the local reuse of the port number:
				{reuseaddr,true},
				
				% Enables periodic transmission on the socket, to avoid the
				% connection is considered broken if idle too long:
				{keepalive,true}
				
			]),

			{ok,ClientHost} = inet:peername(ClientSocket),
			
			% TO-DO: store some reverse-DNS information with 'host':
			% > host 82.225.152.215
			% 215.152.225.82.in-addr.arpa domain name pointer esperide.com.
			% if grep pointer else "unknown"...
			% host 82.225.152.215|sed 's|.*pointer ||1' | sed 's|.$||1'
			
			ManagerState = #manager_state{
				client_login = not_tried_yet,
				client_host = ClientHost,
				manager_version = ?manager_version,
				oldest_supported_client_version =
					?oldest_supported_client_version,
			    starting_time = basic_utils:get_timestamp(),
				client_socket = ClientSocket,
				server_pid = ServerPid,
				connection_identifier = ConnectionIdentifier,
				database_pid = DatabasePid
			},
			% TO-DO:
			% VersionState = check_versions( ManagerState ),
			
			% Useful to introduce random delays:
			% (seeding necessary otherwise always the same values)
			{A1,A2,A3} = now(),
			random:seed( A1, A2, A3 ),
			
			control_access( ManagerState );
			
		{error,closed} ->
			?emit_trace([ "Unable to accept a connection on "
				"listening socket, server must have shut it down, "
				"stopping this client manager now." ])
	
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
	receive
	
		orge_server_shutdown ->
			on_server_shutdown(ManagerState);
			% No recursive call here.
		
		{tcp,ClientSocket,Data} ->
			% Thanks to initial header, we received here a full packet.
			% First message from client must be login informations.
			%?emit_debug([ io_lib:format( 
			%	"Client manager received following TCP message: ~w.", 
			%	[Data] ) ]),
			Id = binary_to_list(Data),
			Identifiers = string:tokens( Id, ?default_identifier_separator ),
			% Precise reasons of failures not reported to the client to avoid
			% too straightforward attacks:
			case Identifiers of 
			
				[Login,Password] ->
					
					case declare_identifiers( ManagerState, Login, Password ) of
						
						access_granted ->
							?emit_trace([ io_lib:format( 
								"Connection acccepted for login '~s' and "
								"password '~s'.", [Login,Password] ) ]),
							gen_tcp:send( ClientSocket,	<<?access_granted>> ),
							loop( ?getState{ client_login = access_granted } );
							
						bad_login ->
							?emit_warning([ io_lib:format( 
								"Connection refused for login '~s' and "
								"password '~s': bad (unregistered) login.",
								 [Login,Password] ) ]),
							gen_tcp:send( ClientSocket,	<<?access_denied>> ),
							gen_tcp:close( ?getState.client_socket ),
							?getState.server_pid ! {self(),closed};
							% No loop here.	
							
						bad_password ->
							?emit_warning([ io_lib:format( 
								"Connection refused for login '~s' and "
								"password '~s': registered login, but "
								"incorrect password.",
								 [Login,Password] ) ]),
							gen_tcp:send( ClientSocket,	<<?access_denied>> ),
							gen_tcp:close( ?getState.client_socket ),
							?getState.server_pid ! {self(),closed};
							% No loop here.	
							
						already_connected ->
							?emit_warning([ io_lib:format( 
								"Connection refused for login '~s' and "
								"password '~s': correct login and password, "
								"but account already in use.",
								 [Login,Password] ) ]),
							gen_tcp:send( ClientSocket,	<<?already_connected>>),
							gen_tcp:close( ?getState.client_socket ),
							?getState.server_pid ! {self(),closed};
							% No loop here.	
														
						account_not_active ->
							?emit_warning([ io_lib:format( 
								"Connection refused for login '~s' and "
								"password '~s': correct login and password, "
								"but account not active.",
								 [Login,Password] ) ]),
							gen_tcp:send( ClientSocket,	<<?access_denied>>),
							gen_tcp:close( ?getState.client_socket ),
							?getState.server_pid ! {self(),closed};
							% No loop here.								
				
						internal_error ->
							?emit_warning([ io_lib:format( 
								"Connection refused for login '~s' and "
								"password '~s': internal error.",
								 [Login,Password] ) ]),
							gen_tcp:send( ClientSocket,	<<?access_denied>>),
							gen_tcp:close( ?getState.client_socket ),
							?getState.server_pid ! {self(),closed}
							% No loop here.	

					end;
					
				_IdentifierList ->
					% Parsing did not result in exactly two elements:
					?emit_error([ io_lib:format( 
						"Error, unable to determine login identifiers "
						"in '~w', connection refused.", [Identifiers] ) ]),
						
					?getState.database_pid ! { declare_marshalling_failed,
						?getState.connection_identifier, 
						?getState.client_host },	
					
					% Waits for a random duration between 1 and 2 seconds:
					timer:sleep( 1000 + random:uniform(1000) ),
							
					gen_tcp:send( ClientSocket, <<?ill_formatted_identifiers>>),
					gen_tcp:close( ?getState.client_socket ),
						
					?getState.server_pid ! {self(),closed}
					% No loop here.
					
			end;		
			
			
		Any ->
		
			?emit_debug([ io_lib:format( "Client manager received "
				"unhandled message while not identified: ~w (ignored).",
				[Any] ) ]),								
			control_access( ManagerState )
	
	
	% Milliseconds:
	after 5000 ->
	
		?emit_warning([ "Time-out while waiting for the client identification, "
			"notifying the client and the server, and "
			"stopping this client manager."
			]),
			
		?getState.database_pid	! { declare_timeout, 
			?getState.connection_identifier, ?getState.client_host },
			
		gen_tcp:send( ?getState.client_socket, <<?timed_out>> ), 
		gen_tcp:close( ?getState.client_socket ),
		?getState.server_pid ! {self(),closed}
		% No loop here.
				
	end.



% Main loop of this client manager, once the client has been identified.
% ManagerState is the current state of this manager (ex: containing the name
% under which the server is registered and all other informations).
loop( ManagerState ) ->
	ClientSocket = ?getState.client_socket,
	receive
	
		orge_server_shutdown ->
			?emit_info([ "Received a shutdown notification from server, "
				"notifying the client and stopping now." ]),
			gen_tcp:send( ClientSocket, <<?shutdown_notification>> ),
			gen_tcp:close( ?getState.client_socket ),
			?getState.server_pid ! {self(),closed};
			% No loop here.
		
		{tcp_closed,ClientSocket} ->
			?emit_debug([ "Client connection closed, "
				"notifying database and server." ]),
				
			?getState.database_pid	! { declare_end_of_session, 
				?getState.connection_identifier },
				
			?getState.server_pid ! {self(),closed};
			% No loop here.
			
		{tcp_closed,Other} ->
			?emit_debug([ io_lib:format(  
				"Unexpected closing of client connection received "
				"(from ~w instead of from ~w), ignored.", 
				[Other,ClientSocket] ) ]),
			loop( ManagerState );	
			
		Any ->
			?emit_debug([ io_lib:format( "Client manager received "
				"unhandled message while identified: ~w (ignored).",
				[Any] ) ]),
			loop( ManagerState )
	
	end.



on_server_shutdown(ManagerState) ->
	?emit_info([ "Received a shutdown notification from server, "
		"notifying client and stopping now." ]),
	gen_tcp:send( ?getState.client_socket, <<?shutdown_notification>> ),
	gen_tcp:close( ?getState.client_socket ).
	
	
	
% Checks that specified login/password are correct,and not already in use.
% Registers in all cases the connection in database.	
declare_identifiers( ManagerState, Login, Password ) ->
	ConnectionId = ?getState.connection_identifier,
	{ClientIP,ClientPort} = ?getState.client_host,
	?emit_debug([ io_lib:format( "Received login '~s' and password '~s', "
		"checking database from connection ~B for ~s.", 
		[ Login, Password, ConnectionId,
			basic_utils:ipv4_to_string(ClientIP,ClientPort) ] ) ]),
		
	?getState.database_pid	! { try_login, {Login,Password}, ConnectionId,
		?getState.client_host, self() },
	receive
	
		{ConnectionId,Answer} ->
			Answer
			
	end.
		

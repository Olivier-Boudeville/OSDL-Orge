% Server-side module dedicated to the management of a client connection.
% Features:
%  - functional behaviour (client management) uncoupled from server
% implementation
%  - transaction semantics
%  - database integration
%
-module(orge_client_manager).

-export([init/4]).


% For manager_state:
-include("orge_client_manager.hrl").

% For default_identifier_separator:
-include("orge_tcp_server.hrl").


% For emit_*:
-include("traces.hrl").



% Internal record that stores the current state of the server-side manager of 
% a client connection:
% client_host is {IPv4,Port}.
-record( manager_state, {
	client_login,
	client_host,
	starting_time,
	client_socket,
	server_pid,
	connection_identifier,
	database_pid          
} ).


% Shortcut macro, for convenience: 
-define(getState,ManagerState#manager_state).



% Inits the client socket.
init(ServerPid,ListenSocket,ConnectionIdentifier,DatabasePid) ->

	?emit_debug([ io_lib:format( 
		"Client manager ~w waiting for next connection #~s.", 
			[self(),utils:integer_to_string(ConnectionIdentifier)] ) ]),
			
	% This process will be the one controlling the client socket once created:	
	case gen_tcp:accept(ListenSocket) of
	
		{ok,ClientSocket} ->
			?emit_debug([ "Connection accepted." ]),
			{ok,ClientHost} = inet:peername(ClientSocket),
			ServerPid ! {self(),accepted},
			ManagerState = #manager_state{
				client_login = not_tried_yet,
				client_host = ClientHost,
			    starting_time = utils:get_timestamp(),
				client_socket = ClientSocket,
				server_pid = ServerPid,
				connection_identifier = ConnectionIdentifier,
				database_pid = DatabasePid
			},
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
							?getState.server_pid ! {self(),closed};
							% No loop here.	
							
						bad_password ->
							?emit_warning([ io_lib:format( 
								"Connection refused for login '~s' and "
								"password '~s': registered login, but "
								"incorrect password.",
								 [Login,Password] ) ]),
							gen_tcp:send( ClientSocket,	<<?access_denied>> ),
							?getState.server_pid ! {self(),closed};
							% No loop here.	
							
						already_connected ->
							?emit_warning([ io_lib:format( 
								"Connection refused for login '~s' and "
								"password '~s': correct login and password, "
								"but account already in use.",
								 [Login,Password] ) ]),
							gen_tcp:send( ClientSocket,	<<?already_connected>>),
							?getState.server_pid ! {self(),closed};
							% No loop here.	
														
						account_not_active ->
							?emit_warning([ io_lib:format( 
								"Connection refused for login '~s' and "
								"password '~s': correct login and password, "
								"but account not active.",
								 [Login,Password] ) ]),
							gen_tcp:send( ClientSocket,	<<?access_denied>>),
							?getState.server_pid ! {self(),closed};
							% No loop here.								
				
						internal_error ->
							?emit_warning([ io_lib:format( 
								"Connection refused for login '~s' and "
								"password '~s': internal error.",
								 [Login,Password] ) ]),
							gen_tcp:send( ClientSocket,	<<?access_denied>>),
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
							
					gen_tcp:send( ClientSocket, <<?ill_formatted_identifiers>>),
						
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
			
		gen_tcp:send(?getState.client_socket,<<?timed_out>>), 
		
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
			gen_tcp:send( ClientSocket, <<?shutdown_notification>> );
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
	gen_tcp:send( ?getState.client_socket, <<?shutdown_notification>> ).
	
	
% Checks that specified login/password are correct,and not already in use.
% Registers in all cases the connection in database.	
declare_identifiers(ManagerState,Login,Password) ->
	ConnectionId = ?getState.connection_identifier,
	{ClientIP,ClientPort} = ?getState.client_host,
	?emit_debug([ io_lib:format( "Received login '~s' and password '~s', "
		"checking database from connection ~B for ~s.", 
		[ Login, Password, ConnectionId,
			utils:ipv4_to_string(ClientIP,ClientPort) ] ) ]),
		
	?getState.database_pid	! { try_login, {Login,Password}, ConnectionId,
		?getState.client_host, self() },
	receive
	
		{ConnectionId,Answer} ->
			Answer
			
	end.
		
	

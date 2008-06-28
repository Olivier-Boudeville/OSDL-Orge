% Server-side module dedicated to the management of a client connection.
% Features:
%  - functional behaviour (client management) uncoupled from server
% implementation
%  - transaction semantics
-module(client_manager).

-export([init/2]).


% For manager_state:
-include("client_manager.hrl").

% For default_identifier_separator:
-include("orge_tcp_server.hrl").


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
		client_socket = ClientSocket,
		server_pid = ServerPid
	},
	control_access( ManagerState ).
	
	
		
% Checks that the associated client is known from the server.
% Protocol:
%   - client sends a packet containing two sequences of bytes separated by
% ?default_identifier_separator
%   - server interprets the first sequence as a login, the second as a password
%   - if the incoming packet is ill-formatted, 
control_access( ManagerState ) ->
	ClientSocket = ManagerState#manager_state.client_socket,
	receive
	
		orge_server_shutdown ->
			on_server_shutdown();
			% No recursive call here.
		
		{tcp,ClientSocket,Data} ->
			%?emit_debug([ io_lib:format( 
			%	"Client manager received following TCP message: ~w.", 
			%	[Data] ) ]),
			Id = binary_to_list(Data),
			Identifiers = string:tokens(Id, ?default_identifier_separator),
			case Identifiers of 
			
				[Login,Password] ->
					case check_identifiers(Login,Password) of 
						
						access_granted ->
							?emit_trace([ io_lib:format( 
								"Connection acccepted for login '~s' and "
								"password '~s'.", [Login,Password] ) ]),
							gen_tcp:send(ClientSocket,<<?access_granted>>),
							loop( ManagerState#manager_state{ 
								client_login = Login } );
							
						access_denied ->
							?emit_warning([ io_lib:format( 
								"Connection refused for login '~s' and "
								"password '~s'.", [Login,Password] ) ]),
							gen_tcp:send(ClientSocket,<<?access_denied>>)
							% No loop here.	

					end;
					
				_Other ->
					?emit_error([ io_lib:format( 
						"Error, unable to determine login identifiers "
						"in '~w', connection refused.", [Identifiers] ) ]),	
					gen_tcp:send(ClientSocket,<<?ill_formatted_identifiers>>)
					% No loop here.
					
			end;		
			
			
		Any ->
			?emit_debug([ io_lib:format( "Client manager received "
				"unhandled message while not identified: ~w (ignored).",
				[Any] ) ]),
			control_access( ManagerState )
	
	% Milliseconds:
	after 5000 ->
	
		?emit_warning([ "Time-out while waiting for the client identification."
			]),
		gen_tcp:send(ClientSocket,<<?timed_out>>) 
		% No loop here.
			
	end.



% Main loop of this client manager, once the client has been identified.
% ManagerState is the current state of this manager (ex: containing the name
% under which the server is registered and all other informations).
loop( ManagerState ) ->
	ClientSocket = ManagerState#manager_state.client_socket,
	receive
	
		{tcp_closed,ClientSocket} ->
			?emit_debug([ "Client connection closed, notifying server." ]),
			?getState.server_pid ! { self(),closed,?getState.client_login, 
				{?getState.starting_time,utils:get_timestamp()} };
			% No loop here.
			
		{tcp_closed,Other} ->
			?emit_debug([ io_lib:format(  
				"Unexpected client connection close received "
				"(~w instead of ~w), ignored.", [Other,ClientSocket] ) ]),
			loop( ManagerState );	
			
		Any ->
			?emit_debug([ io_lib:format( "Client manager received "
				"unhandled message while identified: ~w (ignored).",
				[Any] ) ]),
			loop( ManagerState )
	
	end.


on_server_shutdown() ->
	?emit_info([ "Received a shutdown notification from server, "
		"stopping now." ]).
	
	
% Checks that specified login/password are correct.	
check_identifiers(Login,Password) ->
	?emit_debug([  io_lib:format( "Received login '~s' and password '~s'.",
		[Login,Password] ) ]),
	?emit_trace([ io_lib:format( "Login '~s' and password '~s' accepted.",
		[Login,Password] ) ]),
	access_granted.
	
	
	

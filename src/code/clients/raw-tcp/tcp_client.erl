% Custom-made TCP/IP client, mainly for server testing purpose.
% See tcp_server.erl and tcp_server_test.erl.
-module(tcp_client).



% Implementation notes.

% Inspired from the various clients described in 'Programming Erlang',
% by Joe Armstrong (chapter 16).
% WOOPER has been avoided here to presumably speedup the processings, and 
% as no need for inheritance is expected.

% The role of a client is to contact a server, send commands and receive 
% updates.


% For server defaults:
-include("tcp_server.hrl").


% For emit_*:
-include("traces.hrl").


% Record that stores the current state of a TCP client:
-record( client_state, {
	client_name = undefined,
	client_host = undefined,
	server_host = undefined,
	server_listening_port = undefined,
	communication_socket = undefined
} ).


% Exported API section.

-export([start/3,start_link/3,state_to_string/1]).


% Starts a new TCP client.
%  - ClientName the name under which the client will be known
%  - ServerDNSName a name under which the server can be reached through the 
% DNS
% expected to listen to incoming connections
% Returns the PID of the launched client.
% (static)
start(ClientName,ServerDNSName) ->
	start( ClientName, ServerDNSName, ?default_listening_tcp_server_port ).


% Starts a new TCP client.
%  - ClientName the name under which the client will be known
%  - ServerDNSName a name under which the server can be reached through the 
% DNS
%  - ServerTCPListeningPort the port number of the TCP port the server is
% expected to listen to incoming connections
% Returns the PID of the launched client.
% (static)
start(ClientName,ServerDNSName,ServerTCPListeningPort) ->
	?emit_trace([ io_lib:format( "Starting a TCP client named ~s "
		"that will connect to server ~s on port #~B.",
		[ ClientName, ServerDNSName, ServerTCPListeningPort ] ) ]),
	spawn( fun() ->	init( ClientName, ServerDNSName, ServerTCPListeningPort )
		end ).



% Starts a new TCP client, linked to the calling process.
%  - ClientName the name under which the client will be known
%  - ServerDNSName a name under which the server can be reached through the 
% DNS
% expected to listen to incoming connections
% Returns the PID of the launched client.
% (static)
start_link(ClientName,ServerDNSName) ->
	start_link( ClientName, ServerDNSName, ?default_listening_tcp_server_port ).


% Starts a new TCP client, linked to the calling process.
%  - ClientName the name under which the client will be known
%  - ServerDNSName a name under which the server can be reached through the 
% DNS
%  - ServerTCPListeningPort the port number of the TCP port the server is
% expected to listen to incoming connections
% Returns the PID of the launched client.
% (static)
start_link(ClientName,ServerDNSName,ServerTCPListeningPort) ->
	?emit_trace([ io_lib:format( "Starting a linked TCP client named ~s "
		"that will connect to server ~s on port #~B.",
		[ ClientName, ServerDNSName, ServerTCPListeningPort ] ) ]),
	spawn_link( fun() -> 
		init( ClientName, ServerDNSName, ServerTCPListeningPort ) end ).



% Initializes the TCP client with specified name and server settings, and
% enter its main loop.
init( ClientName, ServerDNSName, ServerTCPListeningPort ) ->

	?emit_debug([ "Client will try to connect now." ]),
		
	% 32-bit header:
	{ok,Socket} = gen_tcp:connect( ServerDNSName, ServerTCPListeningPort,
		[ binary, {packet,4} ] ),

	?emit_debug([ "Client connected, sending a request." ]),
				
	ClientState = #client_state{
		client_name = ClientName,
	    client_host = net_adm:localhost(),
		server_host = ServerDNSName,
		server_listening_port = ServerTCPListeningPort,
		communication_socket = Socket
	},
	loop( ClientState ).
	

% Sends specified request to specified PID or name registered locally.
send_request( ClientPid, Request ) when is_pid(ClientPid) ->
	ClientPid ! {self(),Request},
	receive
	
		{ClientPid,Answer} ->
			Answer;

		{ClientPid,request_failed,Message} ->
			{request_failed,Message}		
	
	end.


send_request_by_name( ClientName, Request ) ->
	send_request( erlang:whereis(ClientName), Request ).
	

% Replaces, for specified client, current version of functional code by
% specified one.
update_functional_code(ClientPid,NewFunctionalModule) ->
	send_request( ClientPid, {update_functional_code,NewFunctionalModule} ).




% Internal API section.


% Main loop of this client.
%  - ClientState is the current state of this client (ex: containing the name
% under which the client is registered)
%  - FunctionalModule the Erlang module that manages the client requests
% and the pushed updates
%  - FunctionalState the state of the functional client 
loop( ClientState, FunctionalModule, FunctionalState ) ->
	?emit_debug([ io_lib:format( "~s~n", [ state_to_string(ClientState) ] ) ]),
	receive
	
		{tcp,ClientSocket,BinMessage} ->
			?emit_debug([ io_lib:format( "Client received binary ~p.~n",
				[BinMessage] ) ]),
			gen_tcp:send( ClientSocket, term_to_binary(packet_received) ),
			?emit_debug([ "Client answered.~n" ]),
			loop( ClientState, FunctionalModule, FunctionalState );

		{tcp_closed,_ClientSocket} ->
			?emit_trace([ "Client closed its socket.~n" ]);
		
		{From,{update_functional_code,NewFunctionalModule}} ->
			From ! {self(),functional_code_swapped},
			loop( ClientState#client_state{
					code_update_count =
						ClientState#client_state.code_update_count + 1,
					last_code_update = utils:get_timestamp() 
				},
				NewFunctionalModule, FunctionalState );

		{From,get_client_state} ->
			From ! {self(),state_to_string(ClientState)},
			loop( ClientState, FunctionalModule, FunctionalState );
	
		{From,Request} ->
			try FunctionalModule:handle( Request, FunctionalState ) of
			
				{Answer,NewFunctionalState} ->
					From ! {self(),Answer},
					loop( ClientState, FunctionalModule, NewFunctionalState )
			
			catch
			
				_:Why ->
					?emit_error([ io_lib:format(
						"Client whose state is ~w failed "
						"while executing request ~w for ~w: ~w.",
						[ClientState,Request,From,Why] ) ]),
						
					% Notify the client:	
					From ! {self(),request_failed,Why},
					
					% Transaction: loop with initial state (no change):
					loop( ClientState, FunctionalModule, FunctionalState )
					
			end				
	
	end.




% Returns a textual description of the specified client state.
state_to_string(ClientState) ->
	io_lib:format( "State of TCP client named ~s:~n"
		"  + started on ~s at ~s~n"
		"  + listening to TCP port ~w~n"
		"  + ~s~n"
		"  + current connections: ~w~n"
		"  + past connections: ~w~n",
		[ 
			ClientState#client_state.client_name,
			ClientState#client_state.host,
			utils:get_textual_timestamp(
				ClientState#client_state.starting_time), 
			ClientState#client_state.listening_port, 
			code_updates_to_string(ClientState),
			ClientState#client_state.current_connections,
			ClientState#client_state.past_connections
		] ).	



% Returns a textual description of past code updates.
code_updates_to_string(ClientState) ->

	case ClientState#client_state.last_code_update of 
		
		undefined ->
			"no functional code update performed yet" ;
			
		TimeStampPair ->
			io_lib:format( "~B functional code update(s) performed, "
				"last one was at ~s",
				[ 
					ClientState#client_state.code_update_count,
					utils:get_textual_timestamp(TimeStampPair) 
				] )
				
	end.
	

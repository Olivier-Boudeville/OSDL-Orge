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


% For server defaults like default_listening_tcp_server_port:
-include("tcp_server.hrl").

% For client informations like the client_state record:
-include("tcp_client.hrl").


% For emit_*:
-include("traces.hrl").


% Shortcut macro, for convenience: 
-define(getState,ClientState#client_state).



% Exported API section.

-export([start/2,start_link/2,start/3,start_link/3,state_to_string/1]).


% Starts a new TCP client.
%  - ClientName the name under which the client will be known
%  - ServerDNSName a name under which the server can be reached through the 
% DNS
% 
% The default TCP listening port number will be used by this client.
%
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
% 
% The default TCP listening port number will be used by this client.
%
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
				
	#client_state{
		client_name = ClientName,
	    client_host = net_adm:localhost(),
	    starting_time = utils:get_timestamp(),
		server_host = ServerDNSName,
		server_listening_port = ServerTCPListeningPort,
		communication_socket = Socket
	},

	?emit_debug([ "Client connected, sending a request." ]).




% Internal API section.



% Returns a textual description of the specified client state.
state_to_string(ClientState) ->
	io_lib:format( "State of TCP client version ~.1f named ~s:~n"
		"  + started on ~s at ~s~n"
		"  + ~s~n",
		[ 
			?getState.client_version,
			?getState.client_name,
			?getState.client_host,
			utils:get_textual_timestamp(?getState.starting_time),
			server_info_to_string(ClientState)
		] ).	

		
	
% Returns a textual description of informations about remote server.
server_info_to_string(ClientState) ->

	IsConnected = case ?getState.communication_socket of 
		
		undefined ->
			"not connected to server" ;
			
		_ ->
			"connected to server"
				
	end,
	
	io_lib:format( "~s ~s:~B", [ IsConnected, ?getState.server_host,
		?getState.server_listening_port ] ).
		

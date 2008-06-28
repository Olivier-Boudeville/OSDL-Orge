% Custom-made test TCP/IP client.
% See tcp_server.erl and tcp_server_test.erl.
-module(orge_tcp_client).



% Implementation notes.

% Inspired from the various clients described in 'Programming Erlang',
% by Joe Armstrong (chapter 16).
% WOOPER has been avoided here to presumably speedup the processings, and 
% as no need for inheritance is expected.

% The role of a client is to contact a server, send commands and receive 
% updates.


% For server defaults like default_listening_tcp_server_port and 
% default_packet_header_size:
-include("orge_tcp_server.hrl").

% For client informations like the client_state record:
-include("orge_tcp_client.hrl").

% For emit_*:
-include("traces.hrl").


% Shortcut macro, for convenience: 
-define(getState,ClientState#client_state).



% Exported API section.

-export([start/3,start_link/3,start/4,start_link/4,state_to_string/1]).


% Starts a new TCP client.
%  - ClientLogin the login under which the client will be known
%  - ClientPassword the password corresponding to the specified login
%  - ServerDNSName a name under which the server can be reached through the 
% DNS
% 
% The default TCP listening port number will be used by this client.
%
% Returns the PID of the launched client.
% (static)
start(ClientLogin,ClientPassword,ServerDNSName) ->
	start( ClientLogin, ClientPassword, ServerDNSName,
		?default_listening_orge_tcp_server_port ).


% Starts a new TCP client.
%  - ClientLogin the login under which the client will be known
%  - ClientPassword the password corresponding to the specified login
%  - ServerDNSName a name under which the server can be reached through the 
% DNS
%  - ServerTCPListeningPort the port number of the TCP port the server is
% expected to listen to incoming connections
% Returns the PID of the launched client.
% (static)
start(ClientLogin,ClientPassword,ServerDNSName,ServerTCPListeningPort) ->
	?emit_trace([ io_lib:format( "Starting a TCP client "
		"that will connect to server ~s on port #~B with login '~s' "
		"and password '~s'.",
		[ ServerDNSName, ServerTCPListeningPort, ClientLogin, ClientPassword ] )
	]),
	spawn( fun() ->	init( ClientLogin, ClientPassword, ServerDNSName,
		ServerTCPListeningPort ) end ).



% Starts a new TCP client, linked to the calling process.
%  - ClientLogin the login under which the client will be known
%  - ClientPassword the password corresponding to the specified login
%  - ServerDNSName a name under which the server can be reached through the 
% DNS
% 
% The default TCP listening port number will be used by this client.
%
% Returns the PID of the launched client.
% (static)
start_link(ClientLogin,ClientPassword,ServerDNSName) ->
	start_link( ClientLogin, ClientPassword, ServerDNSName,
		?default_listening_orge_tcp_server_port ).


% Starts a new TCP client, linked to the calling process.
%  - ClientLogin the login under which the client will be known
%  - ClientPassword the password corresponding to the specified login
%  - ServerDNSName a name under which the server can be reached through the 
% DNS
%  - ServerTCPListeningPort the port number of the TCP port the server is
% expected to listen to incoming connections
% Returns the PID of the launched client.
% (static)
start_link(ClientLogin,ClientPassword,ServerDNSName,ServerTCPListeningPort) ->
	?emit_trace([ io_lib:format( "Starting a linked TCP client "
		"that will connect to server ~s on port #~B with login '~s' "
		"and password '~s'.",
		[ ServerDNSName, ServerTCPListeningPort, ClientLogin, ClientPassword ] )
	]),
	spawn_link( fun() -> 
		init( ClientLogin, ClientPassword, ServerDNSName, 
			ServerTCPListeningPort ) end ).



% Initializes the TCP client with specified name and server settings, and
% connects with appropriate informations.
init( ClientLogin, ClientPassword, ServerDNSName, ServerTCPListeningPort ) ->

	?emit_debug([ "Client will try to connect now." ]),
		
	{ok,Socket} = gen_tcp:connect( ServerDNSName, ServerTCPListeningPort,
		[ binary, {packet,?default_packet_header_size} ] ),
				
	ClientState = #client_state{
		client_login = ClientLogin,
		client_password = ClientPassword,
	    client_host = net_adm:localhost(),
	    starting_time = utils:get_timestamp(),
		server_host = ServerDNSName,
		server_listening_port = ServerTCPListeningPort,
		communication_socket = Socket
	},

	?emit_debug([ "Client connected." ]),
	login(ClientState).


login(ClientState) ->
	?emit_debug([ "Sending a login request." ]),
	Socket = ?getState.communication_socket,	
	Identifiers = list_to_binary( io_lib:format( "~s~s~s", 
		[?getState.client_login, ?default_identifier_separator,
			?getState.client_password] ) ), 
	ok = gen_tcp:send( Socket, Identifiers ),
	?emit_debug([ "Login request sent." ]),
	receive
	
		{tcp,Socket,<<?access_granted>>} ->
			on_successful_login(ClientState);
		
		{tcp,Socket,<<?ill_formatted_identifiers>>} ->
			?emit_fatal( "Server answered: ill formatted identifiers, stopping."
				);
			
		{tcp,Socket,<<?access_denied>>} ->
			?emit_fatal( "Server answered: access denied, "
				"incorrect identifiers." );
			
		{tcp,Socket,<<?timed_out>>} ->
			?emit_fatal( "Server answered: "
				"time-out while waiting for identifiers." )
		
	end.



on_successful_login(_ClientState) ->
	?emit_trace( "Login successful." ).




% Internal API section.



% Returns a textual description of the specified client state.
state_to_string(ClientState) ->
	io_lib:format( "State of Orge TCP client version ~.1f:~n"
		"  + started on ~s at ~s~n"
		"  + ~s~n"
		"  + using login '~s' and password '~s'~n",		
		[ 
			?getState.client_version,
			?getState.client_host,
			utils:get_textual_timestamp(?getState.starting_time),
			server_info_to_string(ClientState),
			?getState.client_login,
			?getState.client_password
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
		

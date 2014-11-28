% Copyright (C) 2003-2013 Olivier Boudeville
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


% Custom-made test TCP/IP client.
% See tcp_server.erl and tcp_server_test.erl.
-module(orge_tcp_client).



% Implementation notes.

% Inspired from the various clients described in 'Programming Erlang', by Joe
% Armstrong (chapter 16).
%
% WOOPER has been avoided here to presumably speedup the processings, and as no
% need for inheritance is expected.

% The role of a client is to contact a server, send commands and receive
% updates.


% For server defaults like default_listening_tcp_server_port and
% default_packet_header_size:
%
-include("orge_tcp_server.hrl").


% For default_identifier_separator:
-include("orge_client_manager.hrl").


% For client informations like the client_state record:
-include("orge_tcp_client.hrl").



% For emit_*:
-include("traces.hrl").



% Record that stores the current state of a TCP client:
-record( client_state, {
		   client_login = undefined,
		   client_password = undefined,
		   client_host = undefined,
		   client_version = ?client_version,
		   starting_time = undefined,
		   server_host = undefined,
		   server_listening_port = undefined,
		   communication_socket = undefined
} ).



% Shortcut macro, for convenience:
-define(getState,ClientState#client_state).




% Exported API section.

-export([ start/3, start_link/3, start/4, start_link/4, state_to_string/1 ]).



% Starts a new TCP client:
%
% - ClientLogin the login under which the client will be known
%
% - ClientPassword the password corresponding to the specified login
%
% - ServerDNSName a name under which the server can be reached through the DNS
%
% The default TCP listening port number will be used by this client.
%
% Returns the PID of the launched client.
%
% (static)
start( ClientLogin, ClientPassword, ServerDNSName ) ->

	start( ClientLogin, ClientPassword, ServerDNSName,
		   ?default_listening_orge_tcp_server_port ).



% Starts a new TCP client:
%
% - ClientLogin the login under which the client will be known
%
% - ClientPassword the password corresponding to the specified login
%
% - ServerDNSName a name under which the server can be reached through the DNS
%
% - ServerTCPListeningPort the port number of the TCP port the server is
%   expected to listen to incoming connections
%
% Returns the PID of the launched client.
%
% (static)
%
start( ClientLogin, ClientPassword, ServerDNSName, ServerTCPListeningPort ) ->

	?notify_trace_fmt( "Starting a TCP client "
					   "that will connect to server ~s on port #~B "
					   "with login '~s' and password '~s'.",
					   [ ServerDNSName, ServerTCPListeningPort,
						 ClientLogin, ClientPassword ]),

	spawn( fun() -> init( ClientLogin, ClientPassword, ServerDNSName,
						  ServerTCPListeningPort ) end ).



% Starts a new TCP client, linked to the calling process:
%
% - ClientLogin the login under which the client will be known
%
% - ClientPassword the password corresponding to the specified login
%
% - ServerDNSName a name under which the server can be reached through the DNS
%
% The default TCP listening port number will be used by this client.
%
% Returns the PID of the launched client.
%
% (static)
%
start_link( ClientLogin, ClientPassword, ServerDNSName ) ->

	start_link( ClientLogin, ClientPassword, ServerDNSName,
				?default_listening_orge_tcp_server_port ).



% Starts a new TCP client, linked to the calling process:
%
% - ClientLogin the login under which the client will be known
%
% - ClientPassword the password corresponding to the specified login
%
% - ServerDNSName a name under which the server can be reached through the DNS
%
% - ServerTCPListeningPort the port number of the TCP port the server is
%   expected to listen to incoming connections
%
% Returns the PID of the launched client.
%
% (static)
%
start_link( ClientLogin, ClientPassword, ServerDNSName,
			ServerTCPListeningPort ) ->

	?notify_trace_fmt( "Starting a linked TCP client that will connect "
					   "to server ~s on port #~B with login '~s' "
					   "and password '~s'.",
					   [ ServerDNSName, ServerTCPListeningPort, ClientLogin,
						 ClientPassword ] ),

	spawn_link( fun() ->
		init( ClientLogin, ClientPassword, ServerDNSName,
			  ServerTCPListeningPort ) end ).



% Initializes the TCP client with specified name and server settings, and
% connects with appropriate informations.
%
init( ClientLogin, ClientPassword, ServerDNSName, ServerTCPListeningPort ) ->

	io:format( "Client ~w created.~n", [ self() ] ),

	?notify_debug( "Client will try to connect now." ),

	case gen_tcp:connect( ServerDNSName, ServerTCPListeningPort,
			[ binary, { packet, ?default_packet_header_size } ] ) of

		{ ok, Socket } ->

			io:format( "Client ~w connected to ~p.~n", [ self(), Socket ] ),

			ClientState = #client_state{
			client_login = ClientLogin,
			client_password = ClientPassword,
			client_host = net_adm:localhost(),
			starting_time = basic_utils:get_timestamp(),
			server_host = ServerDNSName,
			server_listening_port = ServerTCPListeningPort,
			communication_socket = Socket
			},

			?notify_debug( "Client connected, ready for login." ),

			login( ClientState );


		{ error, econnrefused } ->

			?notify_fatal_fmt(
			   "Connection refused (no Orge server running on ~s at port ~s?), "
			   "stopping the client.",
			   [ ServerDNSName,
				 basic_utils:integer_to_string( ServerTCPListeningPort ) ] );


		{ error, OtherError } ->
			?notify_fatal_fmt(
				"Connection failed to server running on ~s at port ~s: ~s, "
				"stopping the client.",
				[ ServerDNSName,
				  basic_utils:integer_to_string( ServerTCPListeningPort ),
				  OtherError ] )

	end.



login( ClientState ) ->

	?notify_debug( "Sending a login request." ),

	Socket = ?getState.communication_socket,

	MarshalledIdentifiers = text_utils:bin_format( "~s~s~s",
		[ ?getState.client_login, ?default_identifier_separator,
		  ?getState.client_password ] ),

	% Uncomment to test the case of too late login (should result on a time-out
	% on the server):
	%
	%timer:sleep( 6000 ),

	case gen_tcp:send( Socket, MarshalledIdentifiers ) of

		ok ->

			?notify_debug( "Login request sent." ),

			receive

				{ tcp, Socket, << ?access_granted >> } ->
					% Only case where the client does not stop:
					on_successful_login( ClientState );

				{ tcp, Socket, << ?ill_formatted_identifiers >> } ->
					?notify_fatal( "Server answered: ill formatted "
								   "identifiers, stopping the client." ),
					on_failed_login( ClientState, ill_formatted_identifiers );

				{ tcp, Socket, << ?access_denied >> } ->
					?notify_fatal( "Server answered: access denied, incorrect "
								   "identifiers, stopping the client." ),
					on_failed_login( ClientState, access_denied );

				{ tcp, Socket, <<?already_connected>> } ->
					?notify_fatal( "Server answered: access denied, "
								   "account already in use." ),
					on_failed_login( ClientState, already_connected );

				{ tcp, Socket, << ?timed_out >> } ->
					?notify_fatal( "Server answered: time-out while waiting "
								   "for identifiers, stopping the client." ),
					on_failed_login( ClientState, timed_out );

				{ tcp, Socket, << ?no_slot_available >> } ->
					% Actually the login informations were not even read:
					?notify_fatal( "Server answered: not available slot, "
								   "terminating the client." ),
					on_failed_login( ClientState, no_slot_available )

			end;


		{ error, closed } ->
			?notify_fatal( "The server closed its socket, presumably due "
						   "to a time-out, stopping the client." )

	end.



% Called as soon as the login has been validated by the server.
%
on_successful_login( ClientState ) ->

	?notify_trace( "Client obtained a server slot and "
				   "its login is successful." ),

	VersionState = check_client_version( ClientState ),

	loop( VersionState ).



% Checking whether client version is not deprecated for the server.
%
check_client_version( ClientState ) ->

	ClientVersion = ?getState.client_version,

	?notify_trace_fmt( "Checking whether client version ~p "
					   "is accepted by server.", [ ClientVersion ] ),

	Socket = ?getState.communication_socket,

	MarshalledVersion = text_utils:string_to_binary(
						  tuple_to_list( ClientVersion ) ),

	% No defensive programming (only ok case):
	case gen_tcp:send( Socket, MarshalledVersion ) of

		ok ->
			?notify_debug( "Client version sent." ),
			receive

				{ tcp, Socket, << ?compatible_version >> } ->
					?notify_trace( "Server acknowledged client version." ),
					loop( ClientState );

				{ tcp, Socket, << ?incompatible_version >> } ->

					?notify_fatal( "Server answered: incompatible client "
								   "version, stopping the client." ),

					throw( { incompatible_client_version, ClientVersion } )

			end

	end.



% This test client is expected to be always requested about the login outcome.
%
on_failed_login( _ClientState, Reason ) ->

	receive

		{ get_login_status, CallerPid } ->

			CallerPid ! { login_failed, Reason },

			?notify_info_fmt( "Client terminates on failed login (~p).",
							  [ Reason ] )

			% Client terminates here.

	end.



loop( ClientState ) ->

	?notify_trace( "Client in main loop." ),

	receive

		{ get_login_status, CallerPid } ->
			CallerPid ! login_success,

			io:format( "Client ~w waiting.~n", [ self() ] ),

			% Waits a bit before terminating:
			%timer:sleep( random:uniform( 2000 ) ),
			timer:sleep( 8000 ),

			terminate_normally( ClientState )
			%loop( ClientState )

	end.



% Client-side connection termination (ex: the user exited from the client).
%
terminate_normally( ClientState ) ->

	?notify_info( "Client now terminating." ),

	io:format( "Client ~w terminating.~n", [ self() ] ),

	Socket = ?getState.communication_socket,

	case gen_tcp:send( Socket, << ?normal_client_side_termination >> ) of

		ok ->
			% No need for the server to acknowledge it:
			?notify_debug( "Termination notification sent." )

	end.



% Internal API section.



% Returns a textual description of the specified client state.
state_to_string( ClientState ) ->

	io_lib:format( "State of Orge TCP client version ~s:~n"
				   "  + started on ~s at ~s~n"
				   "  + ~s~n"
				   "  + using login '~s' and password '~s'~n",
				   [ basic_utils:version_to_string( ?getState.client_version ),
					 ?getState.client_host,
					 basic_utils:get_textual_timestamp( 
					   ?getState.starting_time ),
					 server_info_to_string( ClientState ),
					 ?getState.client_login,
					 ?getState.client_password ] ).



% Returns a textual description of informations about remote server.
%
server_info_to_string( ClientState ) ->

	IsConnected = case ?getState.communication_socket of

		undefined ->
			"not connected to server" ;

		_ ->
			"connected to server"

	end,

	io_lib:format( "~s ~s:~B", [ IsConnected, ?getState.server_host,
								 ?getState.server_listening_port ] ).

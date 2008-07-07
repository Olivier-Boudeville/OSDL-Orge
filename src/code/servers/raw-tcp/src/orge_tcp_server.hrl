
% The default TCP/IP port this TCP server will listen to new connections:
% (higher than 1024 thus non-privileged, and not recorded in /etc/services)
-define(default_listening_orge_tcp_server_port,9512).

% The default maximum number of pending connections on the listening socket:
-define(default_backlog,20).



% The default minimum TCP/IP port in the reserved range for per-client
% connections:
-define(default_min_client_tcp_port,51000).


% The default maximum TCP/IP port in the reserved range for per-client
% connections:
-define(default_max_client_tcp_port,51999).


% Default TCP client range is hence 51999-51000+1 = 1000 TCP clients


% The default size of the header of exchanged TCP applicative messages:
% (32-bit header)
-define(default_packet_header_size,4).




% Section dedicated to access control.


% Default login/password separator:
-define(default_identifier_separator,"|").


% Sent by the server to a client to acknowledge a successful login:
-define(access_granted,0).

% Sent by the server to a client to notify it the identifiers could not be
% parsed:
-define(ill_formatted_identifiers,1).

% Sent by the server to a client to notify it the identifiers were not correct:
-define(access_denied,2).

% Sent by the server to a client to notify it the identifiers were not received
% on time:
-define(timed_out,3).




% The version of this TCP server.
-define(server_version,0.1).



% Internal record that stores the current state of a TCP server:
-record( server_state, {
	server_name = undefined,
	host = undefined,
	server_version = ?server_version,
	starting_time = undefined,
	current_client_module = undefined,
	client_code_update_count = 0,
	last_client_code_update = undefined,
	listening_socket = undefined,
	listening_port = undefined,
	waiting_managers = [],
	accepted_connections = [],
	past_connections = []
} ).


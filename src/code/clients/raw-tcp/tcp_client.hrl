
% The version of this TCP client.
-define(client_version,0.1).


% Record that stores the current state of a TCP client:
-record( client_state, {
	client_name = undefined,
	client_host = undefined,
	client_version = ?client_version,
	starting_time = undefined,
	server_host = undefined,
	server_listening_port = undefined,
	communication_socket = undefined
} ).



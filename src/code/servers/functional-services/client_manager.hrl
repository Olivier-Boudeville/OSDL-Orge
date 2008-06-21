

% Internal record that stores the current state of the server-side manager of 
% a client connection:
-record( manager_state, {
	client_name = undefined,
	client_host = undefined,
	starting_time = undefined,
	client_socket = undefined
} ).


% Shortcut macro, for convenience: 
-define(getState,ManagerState#manager_state).


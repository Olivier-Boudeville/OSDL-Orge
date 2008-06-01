
% The default TCP/IP port this TCP server will listen to new connections:
% (higher than 1024 thus non-privileged, and not recorded in /etc/services)
-define(default_listening_tcp_server_port,9512).

% The default maximum number of pending connections on the listening socket:
-define(default_backlog,20).



% The default minimum TCP/IP port in the reserved range for per-client
% connections:
-define(default_min_client_tcp_port,51000).


% The default maximum TCP/IP port in the reserved range for per-client
% connections:
-define(default_max_client_tcp_port,51999).


% Default TCP client range is hence 51999-51000+1 = 1000 TCP clients


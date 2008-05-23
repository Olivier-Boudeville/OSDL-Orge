
% The default name under which the Orge server process is to be registered:
-define(default_orge_server_name,main_orge_server).

% The default TCP/IP port the Orge server will listen to new connections:
% (higher than 1024 thus non-privileged, and not recorded in /etc/services)
-define(default_orge_server_tcp_port,9512).


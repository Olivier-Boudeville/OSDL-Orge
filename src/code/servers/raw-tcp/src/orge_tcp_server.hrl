% 
% Copyright (C) 2003-2009 Olivier Boudeville
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



% The default module for client management.
-define(default_client_management_module,orge_client_manager).

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


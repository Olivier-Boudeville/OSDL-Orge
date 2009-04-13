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



% Section dedicated to the exchanges between a client manager and its client.



% Subection dedicated to access control.

% Default login/password separator:
-define(default_identifier_separator,"|").


% Sent by the server to a client to acknowledge a successful login:
-define(access_granted,0).

% Sent by the server to a client to notify it the identifiers could not be
% parsed (marshalling_failed):
-define(ill_formatted_identifiers,1).

% Sent by the server to a client to notify it the identifiers were not correct:
% (bad_login/bad_password)
-define(access_denied,2).

% Sent by the server to a client to notify it the corresponding account is 
% already in use:
-define(already_connected,3).

% Sent by the server to a client to notify it the identifiers were not received
% on time:
-define(timed_out,4).



% Notifications from server/client manager to clients.

-define(shutdown_notification,5).


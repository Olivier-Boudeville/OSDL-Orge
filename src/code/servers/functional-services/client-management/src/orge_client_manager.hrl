
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


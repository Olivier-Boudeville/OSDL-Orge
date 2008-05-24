% Custom-made TCP/IP server.
-module(tcp_server).



% Implementation notes.

% Main objective is to uncouple the technical server from the functional one,
% so that one can be modified with no impact on the other.
% Not using OTP gen_server, to learn how to do it and, maybe, to improve 
% performances, ease of integration and adequation to the Orge specific needs
% (ex: contrary to the usual client/server approach, the Orge server has to
% "push" (send to the clients) simulation updates without waiting for a specific
% request to do so.
% Inspired from the various servers described in 'Programming Erlang',
% by Joe Armstrong (chapter 16).
% WOOPER has been avoided here to presumably speedup the processings.

% The role of a server is simply to break incoming data stream into appropriate
% client requests, and to send them updates.


-export([start/3,start_link/3,send_request/2]).


% Starts a new TCP server.
%  - ServerName the name under which the server will be registered
%  - RegistrationType is in 'local_only', 'global_only', 'local_and_global', 
% depending on what kind of registration is requested for this server
%  - FunctionalModule the Erlang module that will manage the client requests
% and the pushed updates 
% Returns the PID of the launched server.
% (static)
start(ServerName,RegistrationType,FunctionalModule) ->
	ServerPid = spawn( fun() ->
		loop( ServerName, FunctionalModule, FunctionalModule:init() ) end ),
	ok = utils:register_as( ServerPid, ServerName, RegistrationType ),
	ServerPid.


% Starts a new TCP server, linked to the calling process.
%  - ServerName the name under which the server will be registered
%  - RegistrationType is in 'local_only', 'global_only', 'local_and_global', 
% depending on what kind of registration is requested for this server
%  - FunctionalModule the Erlang module that will manage the client requests
% and the pushed updates 
% Returns the PID of the launched server.
% (static)
start_link(ServerName,RegistrationType,FunctionalModule) ->
	ServerPid = spawn_link( fun() ->
		loop( ServerName, FunctionalModule, FunctionalModule:init() ) end ),
	ok = utils:register_as( ServerPid, ServerName,RegistrationType ),
	ServerPid.


% Sends specified request to specified name registered locally.
send_request( ServerName, Request ) ->
	ServerName ! {self(),Request},
	receive
	
		{ServerName,Answer} ->
			Answer
	
	end.



% Main loop of this server.
%  - ServerName the name under which the server is registered
%  - FunctionalModule the Erlang module that manages the client requests
% and the pushed updates
%  - FunctionalState the state of the functional server 
loop( ServerName, FunctionalModule, FunctionalState ) ->
	receive
	
		{From,Request} ->
			{Answer,NewFunctionalState} = FunctionalModule:handle( Request,
				FunctionalState ),
				From ! {ServerName,Answer},
				loop( ServerName, FunctionalModule, NewFunctionalState )
	
	end.



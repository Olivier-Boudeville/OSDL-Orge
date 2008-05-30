% Custom-made TCP/IP server.
% Features:
%  - functional behaviour uncoupled from server implementation
%  - transaction semantics
%  - hot code update
%  - simple state management
%  - based on Pid rather than on registered names
-module(tcp_server).



% Implementation notes.

% Main objective is to uncouple the technical server from the functional one,
% so that one can be modified with no impact on the other.
% Not using OTP gen_server, to learn how to do it and, maybe, to improve 
% performances, ease of integration or adequation to the Orge specific needs
% (ex: contrary to the usual client/server approach, the Orge server has to
% "push" (send to the clients) simulation updates without waiting for a specific
% request to do so).
% Inspired from the various servers described in 'Programming Erlang',
% by Joe Armstrong (chapter 16).
% WOOPER has been avoided here to presumably speedup the processings, and 
% as no need for inheritance is expected.

% The role of a server is simply to break incoming data stream into appropriate
% client requests, and to send them updates.


-record( server_state, {server_name} ).


% Exported API section.

-export([start/3,start_link/3,send_request/2,send_request_by_name/2,
	update_functional_code/2]).


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
		loop( #server_state{ server_name = ServerName }, 
			FunctionalModule, FunctionalModule:init() ) end ),
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
		loop( #server_state{ server_name = ServerName },
			FunctionalModule, FunctionalModule:init() ) end ),
	ok = utils:register_as( ServerPid, ServerName,RegistrationType ),
	ServerPid.


% Sends specified request to specified PID or name registered locally.
send_request( ServerPid, Request ) when is_pid(ServerPid) ->
	ServerPid ! {self(),Request},
	receive
	
		{ServerPid,Answer} ->
			Answer;

		{ServerPid,request_failed,Message} ->
			exit( {request_failed,Message} )		
	
	end.


send_request_by_name( ServerName, Request ) ->
	send_request( erlang:whereis(ServerName), Request ).
	

% Replaces, for specified server, current version of functional code by
% specified one.
update_functional_code(ServerName,NewFunctionalModule) ->
	send_request( ServerName, {update_functional_code,NewFunctionalModule} ).




% Internal API section.


% Main loop of this server.
%  - ServerState is the current state of this server (ex: containing the name
% under which the server is registered)
%  - FunctionalModule the Erlang module that manages the client requests
% and the pushed updates
%  - FunctionalState the state of the functional server 
loop( ServerState, FunctionalModule, FunctionalState ) ->
	receive
	
		{From,{update_functional_code,NewFunctionalModule}} ->
			From ! {self(),functional_code_swapped},
			loop( ServerState, NewFunctionalModule, FunctionalState );
	
		{From,Request} ->
			try FunctionalModule:handle( Request, FunctionalState ) of
			
				{Answer,NewFunctionalState} ->
					From ! {self(),Answer},
					loop( ServerState, FunctionalModule, NewFunctionalState )
			
			catch
			
				_:Why ->
					io:format( "Server whose state is ~w failed "
						"while executing request ~w for ~w: ~w.",
						[ServerState,Request,From,Why] ),
						
					% Notify the client:	
					From ! {self(),request_failed,Why},
					
					% Transaction: loop with initial state (no change):
					loop( ServerState, FunctionalModule, FunctionalState )
					
			end				
	
	end.


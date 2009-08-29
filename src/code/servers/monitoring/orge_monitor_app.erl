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


% Monitoring tool for remote Orge servers.
% See the orge_tcp_server.
-module(orge_monitor_app).


% For trace_aggregator_name:
-include("class_TraceAggregator.hrl").


-export([ exec/0 ] ).



% Executes the application.
exec() ->
	
	io:format( "~n    Orge Server Monitor.~n" ),

	{ TargetHost, TargetNode, TargetCookie } =
		orge_tcp_server:get_server_defaults(),

	io:format( "Monitoring the Orge server running on host '~s' "
		"using node name '~s' and cookie ~p.~n", 
			[TargetHost,TargetNode,TargetCookie] ),
	
	erlang:set_cookie( node(), TargetCookie ),
	
	case net_utils:ping( TargetHost ) of
	
		true ->
			io:format( "  Successful ping of host '~s'.~n", [TargetHost] );
			
		false ->
			io:format( "  Warning: ping failed for host '~s', "
				"continuing anyway.~n", [TargetHost] )
			
	end,
	
	TargetNodeName = list_to_atom( TargetNode ++ "@" ++ TargetHost ),
	case net_adm:ping( TargetNodeName ) of
	
		pong ->
			io:format( "  Successful ping of node '~s'.~n", [TargetNodeName] );
			
		pang ->
			throw( {node_ping_failed,TargetNodeName} )
			
	end,
	
	io:format( "  Retrieving now the PID of the TCP server.~n" ),
	ServerPid = basic_utils:wait_for_global_registration_of( orge_tcp_server ),
	
	io:format( "  Retrieving now the current server state.~n~n" ),
	ServerPid ! {self(),get_info},
	receive
	
		{server_info,StateDescription} ->
		io:format( "  ~s~n", [StateDescription] )

	end,
	
	
	io:format( "  Launching the table viewer.~n" ),

	io:format( "  Hint: type 'CTRL-N', then select '~s', "
		"then 'CTRL-M' to select the tables to review.~n", [TargetNodeName] ),
		
	tv:start(),

	io:format( "  Launching the trace supervisor.~n" ),
	
	AggregatorPid = case global:whereis_name( ?trace_aggregator_name ) of
	
		Pid when is_pid(Pid) ->
			Pid
	
	end,
	
	LocalTraceListener =
		class_TraceListener:synchronous_new_link(AggregatorPid),
	
	receive

		{wooper_result,monitor_ok} ->
			io:format( "  Trace monitoring finished.~n" )

	end,
	
	HaltOnExist = true,
	
	case HaltOnExist of
	
		true ->	
			LocalTraceListener ! delete,
			erlang:halt();
			
		false ->
			io:format( "  (leaving an available Erlang shell).~n" )
	
	end.		
	



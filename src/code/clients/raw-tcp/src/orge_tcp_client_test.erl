% Copyright (C) 2003-2010 Olivier Boudeville
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


% Custom-made test TCP/IP Orge client.
% See orge_tcp_client.erl and orge_tcp_server_test.erl.
-module(orge_tcp_client_test).


-define(Tested_modules, [orge_tcp_client] ).


% For trace facilities:
-include("traces_for_tests.hrl").



	
run() ->

	% Using TCP/IP, hence we do not have to rely on node and cookie information:
	{ TargetHost, _TargetNode, _TargetCookie } =
		orge_tcp_server:get_server_defaults(),
		
	?test_start,
	
	?test_info( "A test Orge TCP server is expected to run already, "
		"with default settings (see orge_tcp_server_test.erl)." ),

	?test_info_fmt( "Creating a first new Orge test TCP client, "
		"trying to connect to host ~p.", [TargetHost] ),
	
	% Would fail as in orge_tcp_server_test this account is unregistered:
	%{FirstLogin,FirstPassword} = { "anakin", "Iamy0urfather" },
	
	% This one is valid:
	{FirstLogin,FirstPassword} = { "alf", "hell0Brian" },
	
	FirstClient = orge_tcp_client:start_link( FirstLogin, FirstPassword,
		TargetHost ),
	
	?test_info( "Checking this first client succeeded in connecting." ),
	FirstClient ! {get_login_status,self()},
	receive
	
		login_success ->
			ok
		
	end,
	
	%timer:sleep(10000),
	
	% Can be used to check that multiple connections are indeed refused:
	%{SecondLogin,SecondPassword} = { "alf", "hell0Brian" },
	
	% Can be used to test a bad login:
	%{SecondLogin,SecondPassword} = { "indie", "museum" },
	
	{SecondLogin,SecondPassword} = { "indiana", "museum" },
	
	% If the maximum number of connections for the server is 1, then
	% no more client managers will be spawned and the next call will hang,
	% as no accept will be done.  
	?test_info( "Creating a second new Orge test TCP client." ),
	SecondClient = orge_tcp_client:start_link( SecondLogin, SecondPassword, 
		TargetHost ),
	
	?test_info( "Checking this second client succeeded in connecting." ),
	SecondClient ! {get_login_status,self()},
	receive
	
		login_success ->
			ok
		
	end,
	
	%timer:sleep(10000),
	
	?test_info( "Trying to log twice with the account of first client "
	   "(should fail)." ),
	SameAsFirstClient = orge_tcp_client:start_link( FirstLogin, FirstPassword, 
	   TargetHost ),
	
	?test_info("Checking whether the first client failed in connecting twice."),
		
	SameAsFirstClient ! {get_login_status,self()},
	receive
	
	   {login_failed,already_connected} ->
		   ?test_info( "Double login spotted and rejected as expected." )
	
	after 100 ->
		ok			   
	end,
		
		
	
	%timer:sleep(2000),
	
	%?test_info( "Requesting server informations." ),
	%ServerPid ! {self(),get_info},	
	%receive
	
	%	{server_info,StateString} ->
	%		?test_info_fmt( "Current server state is: ~s.",	[StateString] )
			
	%end,
	
	%?test_info( "Requesting the server to shutdown." ),
	%ServerPid ! {self(),shutdown},
	 	
	?test_stop.


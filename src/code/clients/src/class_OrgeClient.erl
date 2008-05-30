% The Orge server in itself: this class is to be used to spawn new server
% instances.
% This top-level module will manage all others.
-module(class_OrgeServer).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_TraceEmitter]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,ServerName,ServerPort).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/2,new_link/2,
	synchronous_new/2,synchronous_new_link/2,construct/3,delete/1).

% Method declarations.
-define(wooper_method_export,start/1,stop/1).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Server.OrgeServer").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").

	
% Constructs a new Orge Server:
%  - ServerName the name of this Orge Server (atom for name registering)
%  - ServerPort TCP port this server should listen to
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_TraceEmitter:construct( State, ServerName ),
		
	% Class-specific:

	% This server will not terminate when an exit signal is received, instead
	% the signal will be transformed into an 'EXIT' message: 
	% (a monitor could be used instead)
	erlang:process_flag( trap_exit, true ),

	StartingState = ?setAttributes( TraceState, [
		{listening_port,ServerPort}, {running,false},
		{trace_categorization,?TraceEmitterCategorization} ] ),
		 
	case global:register_name( ServerName, self() ) of 
	
		yes ->
			?send_debug([ StartingState, io_lib:format(
				"Orge server registered globally under the name ~w", 
				 [ ServerName ] ) ]);
		
		no ->
			?send_error([ StartingState, 
				"Orge server could not be registered." ]),	
			exit( orge_server_could_not_register )
			
	end,
	
	% Registers as well locally:
	true = register( ServerName, self() ),
	StartingState.
	
	
% Overriden destructor.
delete(State) ->

	% Class-specific actions:
	?trace([ "Deleting Orge server." ]),
	
	StoppedState = case ?getAttr(running) of 
	
		true ->
			% Calls possibly-overriden method 'stop':
			{wooper_result,	NewState, orge_server_stopped} =
				executeRequest( State, stop ),
			NewState;
			
		false ->
			State
			
	end,			
	unregister( ?getAttr(name) ),
	global:unregister_name( ?getAttr(name) ),

	?trace([ "Orge server deleted." ]),

	% Then call the direct mother class counterparts and allow chaining:
	class_TraceEmitter:delete(StoppedState).

	
	
% Methods section.


% Starts this Orge server instance.
% (request)
start(State) ->
	case ?getAttr(running) of 
	
		false ->
			?info([ "Orge server started." ]),
			?wooper_return_state_result( ?setAttribute(State,running,true),
				orge_server_started );
		
		true ->
			?warning([ "Orge server requested to start "
				"whereas already running, nothing done." ]),
			?wooper_return_state_result( State,	orge_server_already_running )
			
	end.		
					

	
% Stops this Orge server instance.
% (request)
stop(State) ->
	case ?getAttr(running) of 
	
		true ->
			?info([ "Orge server stopped." ]),
			?wooper_return_state_result( ?setAttribute(State,running,false),
				orge_server_stopped );
		
		false ->
			?warning([ "Orge server requested to stop "
				"whereas not running, nothing done." ]),
			?wooper_return_state_result( State,	orge_server_was_not_running )
			
	end.			
	

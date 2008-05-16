% Creature class, base of all beings.
-module(class_Creature).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_Actor]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,CreatureName,Location).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/2,new_link/2,
	synchronous_new/2,synchronous_new_link/2,construct/3,delete/1).

% Method declarations.
-define(wooper_method_export,act/1,getAge/1,setAge/2,
	getLocation/1,getAbscissa/1).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Orge.Creature").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For location:
-include("space.hrl").


	
% Constructs a new creature:
%  - CreatureName, the name of the creature
%  - Location, a record in-world location 
% Creature starts at age 0.
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, CreatureName ),

	?send_info([ ActorState, io_lib:format( 
		"Creating a new creature whose name is ~s and whose location is ~s.",
		[ CreatureName, space:location_to_string(Location) ] ) ]),
	
	?setAttributes( ActorState, [ {age,0}, {location,Location},
		{trace_categorization,?TraceEmitterCategorization} ] ).
	
	
	
% Overriden destructor.
% Unsubscribing for TimeManager supposed already done, thanks to a termination
% message. 
delete(State) ->
	% Class-specific actions:
	?info([ "Deleting creature." ]),
	% erlang:unlink() not used, as done manager-side. 

	?debug([ "Creature deleted." ]),

	% Then call the direct mother class counterparts and allow chaining:
	class_Actor:delete(State).
	
	
	

% Methods section.


% Returns the age of this creature.
% (request)
getAge(State) ->
	?wooper_return_state_result( State, ?getAttr(age) ).
	
	
% Sets the age of this creature.
% (oneway)
setAge(State,NewAge) ->
	?wooper_return_state_only( ?setAttribute(State,age,NewAge) ).
	
	

% Returns the in-world location of this creature.
% (request)
getLocation(State) ->
	?wooper_return_state_result( State, ?getAttr(location) ).
	
	
% Returns the in-world abscissa of this creature.
% (request)
getAbscissa(State) ->
	Location = ?getAttr(location),
	?wooper_return_state_result( State, Location#location.x ).
	
	
	


% Management section of the actor.
			

% The core of the test actor behaviour.
% (oneway)
act(State) ->

	?info([ "Creature acting" ]),
	
	?wooper_return_state_only(State).
			
	
	
% Section for helper functions (not methods).

	

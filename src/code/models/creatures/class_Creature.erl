% Creature class, base of all beings.
-module(class_Creature).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_Actor,class_Describable,class_Locatable]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,CreatureName,Description,Location).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/3,new_link/3,
	synchronous_new/3,synchronous_new_link/3,construct/4,delete/1).

% Method declarations.
-define(wooper_method_export,act/1,getAge/1,setAge/2).



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
%  - Location, a 'location' record designating an in-world location 
% Creature starts at age 0.
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, CreatureName ),
	DescribableState = class_Describable:construct(ActorState,Description),
	LocatableState = class_Locatable:construct( DescribableState, Location ),

	?send_info([ LocatableState, io_lib:format( 
		"Creating a new creature whose name is ~s, whose description is ~w "
		"and whose location is ~s.",
		[ CreatureName, Description, space:location_to_string(Location) ] ) ]),
	
	?setAttributes( LocatableState, [ {age,0},
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
	
	
	

% Management section of the actor.
			

% The core of the test actor behaviour.
% (oneway)
act(State) ->

	?info([ "Creature acting" ]),
	
	?wooper_return_state_only(State).
			
	
	
% Section for helper functions (not methods).

	

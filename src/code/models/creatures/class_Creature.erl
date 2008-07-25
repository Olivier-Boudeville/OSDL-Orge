% Creature class, base of all beings.
-module(class_Creature).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_Actor,class_Describable,class_Locatable]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,CreatureName,Description,Location,Age).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/4,new_link/4,
	synchronous_new/4,synchronous_new_link/4,construct/5,delete/1).

% Method declarations.
-define(wooper_method_export,act/1,getAge/1,setAge/2).


% Helper functions.
-export([state_to_string/1]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Orge.Creature").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For location:
-include("space.hrl").

-include("class_Creature.hrl").


% Implementation notes:
%
%  - physical_fatigue: current physical fatigue
%  - max_physical_fatigue: physical BFB (Base Fatigue Budget) 
%  - physical_recover_rate: physical FRR (Fatigue Recovery Rate)
%
%  - mental_fatigue: current mental fatigue
%  - max_mental_fatigue: mental BFB (Base Fatigue Budget) 
%  - mental_recover_rate: mental FRR (Fatigue Recovery Rate)
%

	
% Constructs a new creature:
%  - CreatureName, the name of the creature
%  - Description, a description of this creature
%  - Location, a 'location' record designating an in-world location 
%  - Age, the initial age of this creature
% Creature starts with no physical nor mental fatigue.
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, CreatureName ),
	DescribableState = class_Describable:construct(ActorState,Description),
	LocatableState = class_Locatable:construct( DescribableState, Location ),

	?send_info([ LocatableState, io_lib:format( 
		"Creating a new creature whose name is ~s, whose description is ~w "
		"and whose location is ~s.",
		[ CreatureName, Description, space:location_to_string(Location) ] ) ]),
	
	?setAttributes( LocatableState, [ {age,Age},
		{physical_fatigue,0}, {max_physical_fatigue,0},
		{physical_recover_rate,0}, 
		{mental_fatigue,0}, {max_mental_fatigue,0},
		{mental_recover_rate,0}, 
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


% Returns a textual description of the state of this creature.
state_to_string(State) ->
	io_lib:format( "Creature whose name is '~s', whose description is: '~s', "
		"located at ~s, whose age is ~B, whose physical fatigue is ~B/~B, "
		"whose mental fatigue is ~B/~B,",
		[   class_Actor:get_name(State),
			class_Describable:get_description(State),
			class_Locatable:describe_location(State),
			?getAttr(age),
			?getAttr(physical_fatigue),?getAttr(max_physical_fatigue),
			?getAttr(mental_fatigue),?getAttr(max_mental_fatigue) ] ).
	

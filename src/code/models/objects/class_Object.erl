% Object class, base of all instances corresponding to in-world objects.
-module(class_Object).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_Describable,class_TraceEmitter]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,Description,Size,Weight,BaseCreditValue).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/3,new_link/3,
	synchronous_new/3,synchronous_new_link/3,construct/4,delete/1).

% Method declarations.
-define(wooper_method_export,getSize/1,setSize/2,getWeight/1,setWeight/2).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_Describable header:
-define(TraceEmitterCategorization,"Orge.Object").

% Allows to use macros for trace sending:
-include("class_Describable.hrl").


	
% Constructs a new object instance:
%  - Description is a textual description of this object
%  - Size is the volume of this object, expressed notably in terms of volume
% (unit: dm^3, i.e. litres)
%  - Weight is the weight of this object, expressed in kilograms (kg)
%  - BaseCreditValue: the base value, in credits, of this object
% (if applicable)
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_TraceEmitter:construct( State, "An object" ),
	DescribableState = class_Describable:construct( TraceState, Description ),
	
	ReadyState = ?setAttributes( DescribableState, [ 
		{size,Size}, {weight,Weight},
		{trace_categorization,?TraceEmitterCategorization} ] ).

	?send_info([ DescribableState, io_lib:format( 
		"Creating a new locatable whose location is ~s.",
		[ space:location_to_string(Location) ] ) ]),
	
	
	
	
% Overriden destructor.
% Unsubscribing for TimeManager supposed already done, thanks to a termination
% message. 
delete(State) ->
	% Class-specific actions:
	?info([ "Deleting locatable." ]),
	% erlang:unlink() not used, as done manager-side. 

	?debug([ "Object deleted." ]),

	% Then call the direct mother class counterparts and allow chaining:
	class_Describable:delete(State).
	
	
	

% Methods section.

	

% Returns the in-world location of this locatable.
% (request)
getLocation(State) ->
	?wooper_return_state_result( State, ?getAttr(location) ).


% Sets the in-world location of this locatable.
% (oneway)
setLocation(State,NewLocation) ->
	?wooper_return_state_only( ?setAttribute( State, location, NewLocation) ).
	
	
	
% Returns the in-world abscissa of this locatable.
% (request)
getAbscissa(State) ->
	Location = ?getAttr(location),
	?wooper_return_state_result( State, Location#location.x ).

	
% Sets the in-world abscissa of this locatable.
% (oneway)
setAbscissa(State,NewX) ->
	Location = ?getAttr(location),
	?wooper_return_state_only( ?setAttribute( State, location,
		Location#location{x=NewX} ) ).

	

% Returns the in-world ordinate of this locatable.
% (request)
getOrdinate(State) ->
	Location = ?getAttr(location),
	?wooper_return_state_result( State, Location#location.y ).
	
	
% Sets the in-world ordinate of this locatable.
% (oneway)
setOrdinate(State,NewY) ->
	Location = ?getAttr(location),
	?wooper_return_state_only( ?setAttribute( State, location,
		Location#location{y=NewY} ) ).
	
	

% Returns the in-world altitude of this locatable.
% (request)
getAltitude(State) ->
	Location = ?getAttr(location),
	?wooper_return_state_result( State, Location#location.y ).
	
	
% Sets the in-world altitude of this locatable.
% (oneway)
setAltitude(State,NewZ) ->
	Location = ?getAttr(location),
	?wooper_return_state_only( ?setAttribute( State, location,
		Location#location{z=NewZ} ) ).
	

	
	
% Section for helper functions (not methods).

	

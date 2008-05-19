% Locatable class, base of all instances having in-world coordinates.
-module(class_Locatable).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_TraceEmitter]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,Location).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/1,new_link/1,
	synchronous_new/1,synchronous_new_link/1,construct/2,delete/1).

% Method declarations.
-define(wooper_method_export,getLocation/1,setLocation/2,
	getAbscissa/1,setAbscissa/2,getOrdinate/1,setOrdinate/2,
	getAltitude/1,setAltitude/2).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Orge.Locatable").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For location:
-include("space.hrl").


	
% Constructs a new locatable instances, based on a record of an in-world
% location. 
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_TraceEmitter:construct( State, Location ),
	
	?send_info([ TraceState, io_lib:format( 
		"Creating a new locatable whose location is ~s.",
		[ space:location_to_string(Location) ] ) ]),
	
	?setAttributes( TraceState, [ {location,Location},
		{trace_categorization,?TraceEmitterCategorization} ] ).
	
	
	
% Overriden destructor.
% Unsubscribing for TimeManager supposed already done, thanks to a termination
% message. 
delete(State) ->
	% Class-specific actions:
	?info([ "Deleting locatable." ]),
	% erlang:unlink() not used, as done manager-side. 

	?debug([ "Locatable deleted." ]),

	% Then call the direct mother class counterparts and allow chaining:
	class_TraceEmitter:delete(State).
	
	
	

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

	

% Valuable class, for all instance that have a monetary value (in credits).
-module(class_Valuable).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,BaseValue).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/1,new_link/1,
	synchronous_new/1,synchronous_new_link/1,construct/2,delete/1).

% Method declarations.
-define(wooper_method_export,getCreditValue/1,setCreditValue/2).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
%-define(TraceEmitterCategorization,"Orge.Valuable").

% Allows to use macros for trace sending:
%-include("class_TraceEmitter.hrl").


	
% Constructs a new valuable instance, with specified base value, in credits.
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes, then this class-specific actions:	
	?setAttribute( State, base_value, BaseValue ).
	
	
	
% Overriden destructor.
% Unsubscribing for TimeManager supposed already done, thanks to a termination
% message. 
delete(State) ->
	% Class-specific actions:
	%?info([ "Deleting creature." ]),
	% erlang:unlink() not used, as done manager-side. 

	%?debug([ "Valuable deleted." ]),

	% Then call the direct mother class counterparts and allow chaining:
	class_Actor:delete(State).
	
	
	

% Methods section.


% Returns the base value, in credits, of this instance.
% (request)
getCreditValue(State) ->
	?wooper_return_state_result( State, ?getAttr(base_value) ).
	
	
% Sets the base value, in credits, of this instance.
% (oneway)
setCreditValue(State,NewBaseValue) ->
	?wooper_return_state_only( ?setAttribute(State,base_value,NewBaseValue) ).
	
			
	
	
% Section for helper functions (not methods).

	

% Object class, base of all instances corresponding to in-world objects.
-module(class_Object).


% Determines what are the mother classes of this class (if any):
-define(wooper_superclasses,[class_Describable,class_TraceEmitter]).


% Parameters taken by the constructor ('construct'). 
-define(wooper_construct_parameters,Description,Size,Weight,BaseCreditValue).

% Life-cycle related exported operators:
-define(wooper_construct_export,new/4,new_link/4,
	synchronous_new/4,synchronous_new_link/4,construct/5,delete/1).

% Method declarations.
-define(wooper_method_export,getSize/1,setSize/2,getWeight/1,setWeight/2,
	getBaseCreditValue/1,setBaseCreditValue/2,toString/1).



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_Describable header:
-define(TraceEmitterCategorization,"Orge.Object").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Implementation notes:
%
% getDescription/1, setDescription/2 inherited from class_Describable.erl.

	
% Constructs a new object instance:
%  - Description is a textual description of this object
%  - Size is the volume of this object, expressed notably in terms of volume
% (unit: dm^3, i.e. litres), with a floating-point number
%  - Weight is the weight of this object, expressed in kilograms (kg), with a
% floating-point number
%  - BaseCreditValue: the base value, in credits, of this object, if 
% applicable (expressed with an integer); otherwise: undefined.
construct(State,?wooper_construct_parameters) ->

	% First the direct mother classes, then this class-specific actions:
	DescribableState = class_Describable:construct( State, Description ),
	TraceState = class_TraceEmitter:construct( DescribableState, "An object" ),
	
	ReadyState = ?setAttributes( TraceState, [ 
		{size,Size}, {weight,Weight},
		{base_credit_value,BaseCreditValue},
		{trace_categorization,?TraceEmitterCategorization} ] ),

	?send_info([ ReadyState, io_lib:format( "Creating a new object: ~s.",
		[ state_to_string(ReadyState) ] ) ]),
		
	ReadyState.
	
	
	
	
% Overriden destructor.
delete(State) ->
	% Class-specific actions:
	?info([ "Deleting object." ]),

	?debug([ "Object deleted." ]),
	% Then call the direct mother class counterparts and allow chaining:
	TraceState = class_TraceEmitter:delete(State),
	class_Describable:delete(TraceState).
	
	
	

% Methods section.



% Returns the size of this object, as a floating-point number of litres.
% (const request)
getSize(State) ->
	?wooper_return_state_result( State,?getAttr(size) ).

	
% Sets the description of this object.	
% (oneway)
setSize(State,NewSize) ->
	?wooper_return_state_only( ?setAttribute(State,size,NewSize) ).
	
	
	
% Returns the weight of this object, as a floating-point number of kilograms.
% (const request)
getWeight(State) ->
	?wooper_return_state_result( State,?getAttr(weight) ).


% Sets the weight of this object, as a floating-point number of kilograms.	
% (oneway)
setWeight(State,NewWeight) ->	
	?wooper_return_state_only( ?setAttribute(State,weight,NewWeight) ).
	
	

% Returns the base value of this object, as an integer number of credits.
% (const request)
getBaseCreditValue(State) ->
	?wooper_return_state_result( State,?getAttr(base_credit_value) ).


% Sets the base value of this object, as an integer number of credits.
% (oneway)
setBaseCreditValue(State,NewValue) ->	
	?wooper_return_state_only( 
		?setAttribute(State,base_credit_value,NewValue) ).
	
	
	
% Returns a textual description of the state of this object.
% (const request)
toString(State) ->
	?wooper_return_state_result( State, state_to_string(State) ).



% Section for helper functions (not methods).


% Returns a textual description of the state of this object.
state_to_string(State) ->

	ValueMsg = case ?getAttr(base_credit_value) of 
	
		undefined ->
			"not defined";
			
		Amount ->
			io_lib:format( "~s credits", [utils:integer_to_string(Amount)] )
			
	end,			
		
	io_lib:format( "Object whose description is: '~s', "
		"whose size is ~f litres, whose weight is ~f kilograms, "
		"whose base value is ~s",
		[ ?getAttr(description), ?getAttr(size), ?getAttr(weight), ValueMsg ] ).
		

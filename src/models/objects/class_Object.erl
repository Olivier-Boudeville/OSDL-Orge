% Copyright (C) 2003-2013 Olivier Boudeville
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


% Object class, base of all instances corresponding to in-world objects.
-module(class_Object).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses,[ class_Describable, class_TraceEmitter ]).


% Parameters taken by the constructor ('construct'). 
-define( wooper_construct_parameters, Description, Size, Weight,
		 MaximumWearLevel, BaseCreditValue ).


% Life-cycle related exported operators:
-define( wooper_construct_export, new/5, new_link/5, 
		 synchronous_new/5, synchronous_new_link/5,
		 synchronous_timed_new/5, synchronous_timed_new_link/5,
		 remote_new/6, remote_new_link/6, remote_synchronous_new/6,
		 remote_synchronous_new_link/6, remote_synchronous_timed_new/6,
		 remote_synchronous_timed_new_link/6, construct/6, delete/1 ).



% Method declarations.
-define( wooper_method_export, getSize/1, setSize/2, getWeight/1, setWeight/2,
		 getBaseCreditValue/1, setBaseCreditValue/2,
		 isUsable/1, isReparable/1, increaseWearOf/2, toString/1).



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
%  - MaximumWearLevel: the Maximum Wear Level for this object (MWL)
%  - BaseCreditValue: the base value, in credits, of this object, if 
% applicable (expressed with an integer); otherwise: undefined
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes, then this class-specific actions:
	DescribableState = class_Describable:construct( State, Description ),
	TraceState = class_TraceEmitter:construct( DescribableState, "An object" ),
	
	% wear_level is 'Current Wear Level' (CWL).
	ReadyState = setAttributes( TraceState, [ 
		{size,Size}, 
		{weight,Weight},
		{wear_level,0},
		{max_wear_level,MaximumWearLevel},
		{base_credit_value,BaseCreditValue},
		{trace_categorization,
		 text_utils:string_to_binary(?TraceEmitterCategorization)} 
											] ),

	?send_info_fmt( ReadyState, "Creating a new object: ~s.",
		[ state_to_string(ReadyState) ] ),
		
	ReadyState.
	
	
	
	
% Overridden destructor.
delete(State) ->

	% Class-specific actions:
	%?info( "Deleting object." ),

	%?debug( "Object deleted." ),
	% Then call the direct mother class counterparts and allow chaining:
	TraceState = class_TraceEmitter:delete(State),
	class_Describable:delete(TraceState).
	
	

	

% Methods section.



% Returns the size of this object, as a floating-point number of litres.
%
% (const request)
getSize(State) ->
	?wooper_return_state_result( State, ?getAttr(size) ).

	
	
% Sets the description of this object.	
% (oneway)
setSize( State, NewSize ) ->
	?wooper_return_state_only( setAttribute( State, size, NewSize ) ).
	
	
	
% Returns the weight of this object, as a floating-point number of kilograms.
% (const request)
getWeight(State) ->
	?wooper_return_state_result( State, ?getAttr(weight) ).



% Sets the weight of this object, as a floating-point number of kilograms.	
% (oneway)
setWeight(State,NewWeight) ->	
	?wooper_return_state_only( setAttribute( State, weight, NewWeight ) ).
	
	
	

% Returns the base value of this object, as an integer number of credits.
%
% (const request)
getBaseCreditValue(State) ->
	?wooper_return_state_result( State, ?getAttr(base_credit_value) ).



% Sets the base value of this object, as an integer number of credits.
%
% (oneway)
setBaseCreditValue( State, NewValue ) ->	
	?wooper_return_state_only( 
		setAttribute( State, base_credit_value, NewValue ) ).
	
	
	
% Tells whether this object can be effectively used, i.e. if it is actually
% functional.
%
% Returns true or false.
% (const request)
isUsable(State) ->
	?wooper_return_state_result( State, is_usable(State) ).



% Tells whether this object can be repaired, i.e. if it needs to, and is still
% able to be repaired.
%
% Returns true or false.
% (const request)
isReparable(State) ->
	?wooper_return_state_result( State, is_reparable(State) ).
	
	
	
% Adds specified wear level to this object.
%
% (oneway)
increaseWearOf(State,WearToAdd) ->	

	MaxWear = ?getAttr(max_wear_level), 

	% Caps new wear to the maximum one:
	NewWearValue = case ?getAttr(wear_level) + WearToAdd of
	
		Value when Value > MaxWear ->
			MaxWear;
				
		OtherValue ->
			OtherValue
			
	end,
	?wooper_return_state_only( setAttribute( State, wear_level, NewWearValue )
							 ).
	
	
	
% Returns a textual description of the state of this object.
%
% (const request)
toString(State) ->
	?wooper_return_state_result( State, state_to_string(State) ).




% Section for helper functions (not methods).


% Tells whether this object can be effectively used, i.e. if it is actually
% functional.
%
% Returns true or false.
is_usable(State) ->

	case get_wear_percentage(State) of
	
		Value when Value < 0.85 ->
			true;
	
		_Value ->
			false
			
	end.
	
	

% Tells whether this object can be repaired, i.e. if it needs to, and is still
% able to be repaired.
%
% Returns true or false.
is_reparable(State) ->
	MaxWear = ?getAttr(max_wear_level),
	case ?getAttr(wear_level) of
	
		0 ->
			false;
	
		MaxWear ->
			false;
	
		_Value ->
			true
			
	end.
	
	

% Returns the wear percentage for this object.
get_wear_percentage(State) ->
	?getAttr(wear_level) / ?getAttr(max_wear_level).
	
	

% Returns the wear state (atom) for this object.
get_wear_state(State) ->

	% Beware to rounding errors:
	case get_wear_percentage(State) of
	
		0.0 ->
			new;
		
		Value when Value < 0.25	->
			lightly_used;
		
		Value when Value < 0.55	->
			used;
		
		Value when Value < 0.85	->
			worn_out;
		
		Value when Value < 100	->
			broken;
		
		_Value ->
			unreparable	
			
	end.


	
% Returns a textual description of the wear state of this object.
wear_state_to_string(State) ->

	case get_wear_state(State) of
	
		new ->
			"New";
		
		lightly_used ->
			"Lightly Used";
				
		worn_out ->
			"Worn-Out";
			
		broken ->
			"Broken";
			
		unreparable ->
			"Unreparable"
					
	end.
	
	
	
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
		"whose wear state is ~s (~B/~B), whose base value is ~s",
		[ class_Describable:get_description(State), ?getAttr(size),
			?getAttr(weight),wear_state_to_string(State), ?getAttr(wear_level),
			?getAttr(max_wear_level), ValueMsg ] ).
		
		

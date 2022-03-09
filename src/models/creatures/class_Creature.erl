% Copyright (C) 2013-2022 Olivier Boudeville
%
% This file is part of the OSDL-Orge library.
%
% The OSDL-Orge library is free software: you can redistribute it and/or modify
% it under the terms of either the GNU Lesser General Public License or the GNU
% General Public License, as they are published by the Free Software Foundation,
% either version 3 of these Licenses, or (at your option) any later version.
%
% The OSDL-Orge library is distributed in the hope that it will be useful, but
% WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
% FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
% and the GNU General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License and
% of the GNU General Public License along with the OSDL-Orge library. If not,
% see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]


% @doc Creature class, <b>base of all beings</b>.
-module(class_Creature).

-define( class_description, "Class modelling any kind of creature.").

% Determines what are the mother classes of this class (if any):
-define( superclasses, [ class_Actor, class_Locatable ] ).



% Helper functions.
-export([ state_to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Orge.Creature").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For location:
%-include("space.hrl").

-include("class_Creature.hrl").


-type fixme() :: any().


-type creation_name() :: any_string().
% The name of a creature, as specified at creation.


-type name() :: bin_string().
% The (internal) name of a creature.


-type age() :: day_duration().
% The age of a creature, as a number of (full) days.


-type life_points() :: count().
% A number of life points.

-type health() :: life_points().
% The health of a creature.

-type location() :: fixme().
% A location; quite game-specific.

-type mana() :: count().


-type experience_point() :: count().
% An experience point, either global or, preferably, relative to a skill.



% Fatigue section.

-type fatigue() :: fixme().
% A fatigue experienced by a creature.


-type physical_fatigue() :: fatigue().
% A physical fatigue experienced by a creature.

-type mental_fatigue() :: fatigue().
% A mental fatigue experienced by a creature.


-type fatigue_budget() :: fatigue().
% A fatigue budget.


-type recovery_rate() :: fixme().
% A rate of recovery (improvement per unit of time).

-type fatigue_recovery_rate() :: recovery_rate().
% A rate of recovery in terms of fatigue; a.k.a. FRR (Fatigue Recovery Rate).

-type physical_fatigue_recovery_rate() :: fatigue_recovery_rate().
% A rate of recovery in terms of physical fatigue (fatigue decrease per unit of
% time); a.k.a. PFRR (Fatigue Recovery Rate).

-type mental_fatigue_recovery_rate() :: fatigue_recovery_rate().
% A rate of recovery in terms of mental fatigue (fatigue decrease per unit of
% time); a.k.a. MFRR (Mental Fatigue Recovery Rate).


-export_type([ fatigue/0,
			   physical_fatigue/0, mental_fatigue/0,

			   recovery_rate/0,
			   physical_fatigue_recovery_rate/0, mental_fatigue_recovery_rate/0,

			 ]).



-define( class_attributes, [

	{ birth_timestamp, timestamp(),
	  "the timestamp at which this creature was born" },

	{ age, maybe( age() ), "age of this creature (if any computed, typically "
	  "either from date of birth or through its proper time)" },

	{ health, health(), "the current health of this creature" },

	{ max_health, health(), "the maximum health of this creature" },

	{ location, location(), "the current location of this creature" },

	{ physical_fatigue, physical_fatigue(), "current physical fatigue" },

	{ max_physical_fatigue, physical_fatigue(),
	  "physical BFB (Base Fatigue Budget)" },

	{ mental_fatigue, mental_fatigue(), "current mental fatigue" },

	{ max_mental_fatigue, mental_fatigue(),
	  "mental BFB (Base Fatigue Budget)" },

	{ mana, mana(), "the current mana of this creature" },

	{ max_mana, mana(), "the maximum mana of this creature" }

	% Not used at least currently, we prefer skill-relative experience:
	%{ experience, experience_point(),
	%  "the (overall) experience of that creature" };

} ).



% Shorthands:

-type count() :: basic_utils:count().

-type birth_date() :: time_utils:birth_date().
-type timestamp()  : time_utils:timestamp().


% @doc Constructs a new creature:
%  - CreatureName, the creation-time name of this creature
%  - Location, a 'location' record designating an in-world location
%
% Creature starts with no physical nor mental fatigue, and as if it was just
% born.
%
-spec construct( wooper:state(), creation_name(), location() ) ->
										wooper:state().
construct( State, CreationName, Location ) ->

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, CreationName ),
	LocatableState = class_Locatable:construct( ActorState, Location ),

	?send_info_fmt( LocatableState,
		"Creating a new creature whose name is ~ts and whose location is ~ts.",
		[ CreationName, space:location_to_string( Location ) ] ),

	setAttributes( LocatableState, [
		{ age, 0 },
		{ physical_fatigue,0},
		{ max_physical_fatigue,0},
		{ physical_recover_rate,0},
		{ mental_fatigue,0},
		{ max_mental_fatigue,0},
		{ mental_recover_rate,0},
		{ trace_categorization,
			text_utils:string_to_binary(?TraceEmitterCategorization)}
				 ] ).

% onFirstDiasca: birth_timestamp

% @doc Overridden destructor.
delete( State ) ->

	% Class-specific actions:
	?info( "Deleting creature." ),

	?debug( "Creature deleted." ),

	State.




% Methods section.



% @doc Returns the current age of this creature.
-spec getAge( wooper:state() ) -> const_request_return( age() ).
getAge( State ) ->
	wooper:const_return_result( ?getAttr(age) ).



% @doc Sets the current age of this creature.
-spec setAge( wooper:state(), age() ) -> oneway_return().
setAge( State, NewAge ) ->
	wooper:return_state_only( setAttribute( State, age, NewAge ) ).




% Management section of the actor.



% The core of the actor behaviour.
%
% (oneway)
act(State) ->

	?info( "Creature acting" ),

	wooper:return_state_only(State).



% Section for helper functions (not methods).


% @doc Returns a textual description of the state of this creature.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->
	text_utils:format( "Creature whose name is '~ts', whose description "
		"is: '~ts', located at ~ts, whose age is ~B, whose physical "
		"fatigue is ~B/~B, whose mental fatigue is ~B/~B,",
		[ class_Actor:get_name( State ),
		  class_Describable:get_description( State ),
		  class_Locatable:describe_location( State ),
		  ?getAttr(age),
		  ?getAttr(physical_fatigue), ?getAttr(max_physical_fatigue),
		  ?getAttr(mental_fatigue), ?getAttr(max_mental_fatigue) ] ).

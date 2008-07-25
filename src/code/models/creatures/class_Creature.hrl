

% Records the static informations about fatigue.
-record( creature_fatigue_model, {

	% Physical fatigue:
	max_physical_fatigue, 
	physical_recover_rate,
	
	% Mental fatigue:
	max_mental_fatigue,
	mental_recover_rate

} ).



% Records the primary attributes of a creature.
-record( creature_primary_attributes, {

	strength,
	agility,
	constitution,
	intelligence,
	wisdom,
	willpower,
	charisma,
	quickness,
	longevity
	
} ).



% Gender Attribute Modifiers.

-define( strength_male_modifier,       8  ).
-define( agility_male_modifier,       -2  ).
-define( constitution_male_modifier,   5  ).
-define( intelligence_male_modifier,   0  ).
-define( wisdom_male_modifier,        -4  ).
-define( willpower_male_modifier,      3  ).
-define( charisma_male_modifier,       0  ).
-define( quickness_male_modifier,      2  ).
-define( longevity_male_modifier,      0  ).


-define( strength_female_modifier,     0  ).
-define( agility_female_modifier,      2  ).
-define( constitution_female_modifier, 0  ).
-define( intelligence_female_modifier, 0  ).
-define( wisdom_female_modifier,       4  ).
-define( willpower_female_modifier,    0  ).
-define( charisma_female_modifier,     3  ).
-define( quickness_female_modifier,    0  ).
-define( longevity_female_modifier,    10 ).



% Records the secondary attributes of a creature.
-record( creature_secondary_attributes, {

	fatigue_model
	max_nominal_carried_weight
	
} ).



% Records the state attributes of a creature.
-record( creature_state_attributes, {

	age,
	health,
	location,
	physical_fatigue,
	mental_fatigue,
	mana,
	experience
	
} ).



% Records all the statistics of a creature.
-record( creature_characteristics, {

	primary = #creature_primary_attributes{},
	secondary = #creature_secondary_attributes{},
	state = #creature_state_attributes{},

	abilities,
	skills

} ).


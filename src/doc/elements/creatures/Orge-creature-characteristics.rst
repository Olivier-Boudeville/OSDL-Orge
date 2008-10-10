

Characteristics
---------------

The characteristics of a creature are determined by a set of attributes. Most attributes are positive unit-less integers, with no upper limit, although attribute values beyond 100 should be very, very rare.

The characteristics of a creature are split in following attribute sets:

 #. Primary Attributes
 #. Secondary Attributes
 #. State Attributes
 #. Abilities & Traits
 #. Skills

 
All characteristics are declared in class_Creature.hrl_.
 
 

Primary Attributes
..................

The core potential of a creature is described by following *primary attributes*:
 
 
+-------------+-------------------------------------------+-------------------+-------------------+
| Name of the | Meaning and Use [#]_                      | Abbreviation      | Usual Synonyms    |
| Primary     |                                           |                   | and Close Terms   |
| Attribute   |                                           |                   |                   |
+=============+===========================================+===================+===================+
| Strength    | Physical force, vigor, power.             | STR               |                   |
|             | Affects the damages inflicted (notably in |                   |                   |
|             | melee), the weight that can be carried,   |                   |                   |
|             | etc.                                      |                   |                   |
+-------------+-------------------------------------------+-------------------+-------------------+
| Agility     | Power of moving the limbs quickly and     | AGL               | Dexterity,        |
|             | easily; nimbleness.                       |                   | Accuracy,         |
|             | Affects the character's defense (notably  |                   | Stamina           |
|             | its ability to dodge attacks), its ranged |                   |                   |
|             | attacks, etc.                             |                   |                   |
+-------------+-------------------------------------------+-------------------+-------------------+
| Constitution| Ability to withstand fatigue, disease,    | CST               | Endurance,        |
|             | deprivation, etc., and continue working.  |                   | Hardiness         |
|             | Affects most resistances (notably to      |                   |                   |
|             | poison), physical fatigue, etc.           |                   |                   |
+-------------+-------------------------------------------+-------------------+-------------------+
| Intelligence| Readiness of comprehension.               | INT               | I.Q.              |
|             | Affects most Arcane aptitudes (notably    |                   |                   |
|             | Mage ones), decoding and languages skills |                   |                   |
|             |  etc.                                     |                   |                   |
+-------------+-------------------------------------------+-------------------+-------------------+
| Wisdom      | Knowledge, and the capacity to make due   | WIS               | Discernment,      |
|             | of it.                                    |                   | judgment          |
|             | Affects notably checks involving          |                   |                   |
|             | shrewdness, links with divinities and     |                   |                   |
|             | undead, etc.                              |                   |                   |
+-------------+-------------------------------------------+-------------------+-------------------+
| Willpower   | Power of the mind by which we decide to do| WLP               | Will,             |
|             | or not to do.                             |                   | Self-control,     |
|             | Affects notably mental fatigue,           |                   | Courage           |
|             | concentration, resistance to mental       |                   |                   |
|             | arcane powers, etc.                       |                   |                   |
+-------------+-------------------------------------------+-------------------+-------------------+
| Charisma    | Personal attractiveness that enables      | CHR               | Appeal,           |
|             | to influence others.                      |                   | Attractiveness    |
|             | Affects notably the loyalty of allies,    |                   | Authority, Charm, |
|             | the respect of foes, the ability to       |                   | Leadership,       |
|             | convince others, etc.                     |                   | Suggestion        |
+-------------+-------------------------------------------+-------------------+-------------------+
| Quickness   | Rapidity of action (speed) and wit        | QCK               | Speed, Sagacity,  |
|             | (mental alertness).                       |                   | Initiative,       |
|             | Affects initiative in combats, reflexes,  |                   | Sharpness         |
|             | ability to reload ranged weapons, etc.    |                   |                   |
+-------------+-------------------------------------------+-------------------+-------------------+
| Longevity   | Expected length of life.                  | LGV               | Lifespan          |
|             | Affect the latest possible death of the   |                   |                   |
|             | character.                                |                   |                   |
|             |                                           |                   |                   |
+-------------+-------------------------------------------+-------------------+-------------------+


.. [#] Most definitions are taken from various dictionaries, like the Webster.

All primary attributes are positive integers. All, except Longevity, are in the 0-100 range. For the average male adult, each attribute would be roughly equal to 15, which is defined as the ``NeutralPrimaryAttributeValue``.



+-----------+----------+---------+--------------+--------------+--------+
| Species   | Strength | Agility | Constitution | Intelligence | Wisdom |
| Attributes|          |         |              |              |        |
| Modifiers |          |         |              |              |        |
| (1/2)     |          |         |              |              |        |
+===========+==========+=========+==============+==============+========+
| Human     | +0%      | +0%     | +0%          | +0%          | +0%    |
+-----------+----------+---------+--------------+--------------+--------+
| Elf       | -12%     | +8%     | -15%         | +8%          | +10%   |
+-----------+----------+---------+--------------+--------------+--------+
| Dwarf     | +25%     | -8%     | +30%         | -10%         | -8%    |
+-----------+----------+---------+--------------+--------------+--------+
| Halfling  | -15%     | +15%    | -12%         | -2%          | +5%    |
+-----------+----------+---------+--------------+--------------+--------+
| Gnome     | -38%     | +10%    | -18%         | +16%         | -5%    |
+-----------+----------+---------+--------------+--------------+--------+
| Goblin    | +5%      | +6%     | +5%          | -2%          | -10%   |
+-----------+----------+---------+--------------+--------------+--------+
| Orc       | +15%     | -2%     | +8%          | -10%         | -10%   |
+-----------+----------+---------+--------------+--------------+--------+



+-----------+-----------+----------+-----------+-----------+
| Species   | Willpower | Charisma | Quickness | Longevity |
| Attributes|           |          |           |           |
| Modifiers |           |          |           |           |
| (2/2)     |           |          |           |           |
+===========+===========+==========+===========+===========+
| Human     | +0%       | +0%      | +0%       | +0%       |
+-----------+-----------+----------+-----------+-----------+
| Elf       | -12%      | +10%     | +2%       | +80%      |
+-----------+-----------+----------+-----------+-----------+
| Dwarf     | +15%      | -8%      | -5%       | +30%      |
+-----------+-----------+----------+-----------+-----------+
| Halfling  | -2%       | +4%      | +10%      | -8%       |
+-----------+-----------+----------+-----------+-----------+
| Gnome     | -5%       | -14%     | +6%       | -14%      |
+-----------+-----------+----------+-----------+-----------+
| Goblin    | -5%       | -15%     | +5%       | +0%       |
+-----------+-----------+----------+-----------+-----------+
| Orc       | -2%       | -4%      | +8%       | +5%       |
+-----------+-----------+----------+-----------+-----------+




These primary attributes can be dispatched into *physical* ones (Strength, Agility and Constitution) and *mental* ones (Intelligence, Wisdom and Willpower), whereas Charisma and Quickness are in both categories, and Longevity in neither (this particular attribute is seldom used for attribute rolls).


Primary attributes are first-order ones, as they are the true original values from which secondary attributes will be computed.

They are intrinsic, characteristic of a character. The primary attributes can increase during the life of the character (ex: stronger character after training) or decrease (ex: a character suffers from permanent wounds).
Age modifiers apply as well.

At character creation, the player may be given a total number of points to distribute among these primary attributes, or they might be drawn according to a random law. In either case, no primary attribute is allowed be below 10 or above 60. See also: `Character Creation`_.



Other attributes that were not retained here were:

+-------------+-------------------------------------------+
| Name of the | Rejection Reason                          |
| Attribute   |                                           |
+=============+===========================================+
| Piety       | Should be roleplayed                      |
+-------------+-------------------------------------------+
| Vitality    | Too close to stamina                      |
+-------------+-------------------------------------------+
| Luck        | Does not exist as such                    |
+-------------+-------------------------------------------+
| Cunning     | Should be roleplayed                      |
+-------------+-------------------------------------------+
| Valour      | Not relevant                              |
+-------------+-------------------------------------------+



 

Gender Attribute Modifiers
..........................

Gender is associated to primary attributes.

These *Gender Attribute Modifiers* apply independently from the species.


+-------------+---------------+-----------------+
| Name of the | Male Modifier | Female Modifier |
| Primary     |               |                 |
| Attribute   |               |                 |
+=============+===============+=================+
| Strength    | +15%          | +0%             |
+-------------+---------------+-----------------+
| Agility     | -8%           | +8%             |
+-------------+---------------+-----------------+
| Constitution| +10%          | +0%             |
+-------------+---------------+-----------------+
| Intelligence| +0%           | +0%             |
+-------------+---------------+-----------------+
| Wisdom      | -5%           | +5%             |
+-------------+---------------+-----------------+
| Willpower   | +4%           | +0%             |
+-------------+---------------+-----------------+
| Charisma    | +0%           | +3%             |
+-------------+---------------+-----------------+
| Quickness   | +2%           | +0%             |
+-------------+---------------+-----------------+
| Longevity   | 0             | +10%            |
+-------------+---------------+-----------------+




Secondary Attributes
....................


Each creature will have following *secondary attributes*, determined at least partly from primary ones:

+-------------+-------------------------------------------+-------------------+
| Name of the | Meaning and Use [#]_                      | Formula           |
| Secondary   |                                           | parametrized by   |
| Attribute   |                                           | Primary           |
|             |                                           | Attributes        |
+=============+===========================================+===================+
| Health      | The state of being sound and whole, in    | Species, Gender,  |
|             | body, mind, and soul, including being free| Constitution,     |
|             | from disease and pain.                    | Willpower,        |
|             | See `Health`_ for more details.           | Age and Longevity |
+-------------+-------------------------------------------+-------------------+
| Fatigue     | Physical and mental ability to overcome   | See below         |
| Model       | fatigue, by resistance and recovery.      |                   |
+-------------+-------------------------------------------+-------------------+
| Movement    | The maximum walking speed under nominal   | Strength,         |
| Rate        | conditions.                               | Agility           |
+-------------+-------------------------------------------+-------------------+
| Nominal     | The maximum carried weight with no        | Strength,         |
| Carried     | movement penalty. A creature will not be  | Constitution      |
| Weight (NCW)| able to move at all if loaded with        |                   |
|             | ``SpeciesWeightFactor*NCW`` kilograms or  |                   |
|             | higher. See                               |                   |
|             |`Fatigue Due To Carried Weight`_.          |                   |
+-------------+-------------------------------------------+-------------------+
| Height      | The height, in meters, of the creature.   | Species           |
+-------------+-------------------------------------------+-------------------+
| Radius      | The average distance, in meters, between  | Species           |
|             | the center of gravity of this creature and|                   |
|             | each part of its body.|                   |                   |
+-------------+-------------------------------------------+-------------------+


The "size" of a creature is determined from its ``Height`` and ``Radius``: ``Size = Height*Radius``.

The size allows to compute notably the solid angle when targeted by a ranged weapon.




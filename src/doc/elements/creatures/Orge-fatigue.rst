Fatigue
-------

Orge offers a real-time fatigue system. Physical and mental (psychological) exhaustion are internally managed, they are quantified with ``Physical Fatigue Point`` (PFP) and ``Mental Fatigue Point`` (MFP). Note that the latter has nothing to do with mana points, it applies as well to non-magical users.

The current overall state of fatigue and stress of the main character is based on the average of physical and mental fatigue, and reported to the player only by the audio feedback of a heart beating quicker and quicker - until fainting (and likely becoming monster meat). Thus the player has to discriminate himself between the two kinds of fatigue.

Taking damage, or moving too quickly and too much loaded will cause the heart to pulse rapidly, exactly as ambushes and horrific sights will do.

The different causes of fatigue adds up, and may lead to the character exhaustion or unability to act without resting.

The fatigue system is internally managed that way, for each kind of fatigue:

 - each creature has a ``Base Fatigue Budget`` (BFB) and a ``Fatigue Recovery Rate`` (FRR), which are determined by its specy, its statistics (notably its Constitution), its age/lifespan ratio

 - each creature is created with an initial level of fatigue, and its various actions induce a fatigue penalty, depending on many factors including a base cost (depending on the action) and context-depent additional costs which may depend on weight carried, temperature, age, state of health (if the character is wounded, or poisoned, etc.)

 - each round, each creature having non-null fatigue points (FP) have them decreased by FRR

 - a creature is unable to perform any action which would result in having a fatigue budget higher than its BFB



One can picture the fatigue system as a water bucket whose maximum level is the BFB, with a leak equal to the FRR, which starts initially empty but is filled a bit as each action of the character. The goal is to avoid that the water level reaches the top of the bucket.


Fatigue is managed in class_Creature.erl_ and tested in class_Creature_test.erl_.



Fatigue Model
.............


The fatigue model is parametrized by two pairs of values, regarding physical and mental state:

 - maximum sustainable fatigue (``max_physical_fatigue`` and ``max_mental_fatigue``)
 - recovery rate (``physical_recover_rate`` and ``mental_recover_rate``)


The fatigue attributes of a character are determined at character creation, from a base value computed from the primary attributes and affected by various modifiers.


+-------------------------+----------------------------------------------+-------------------+
| Name of the             | Base Value                                   | Potential         |
| Fatigue                 |                                              | Modifiers         |
| Attribute               |                                              |                   |
|                         |                                              |                   |
+=========================+==============================================+===================+
| Maximum Physical        | ``Constitution * 5 + Strength * 2 + Agility``| Species, Traits,  |
| Sustainable Fatigue     |                                              | Buffs             |
+-------------------------+----------------------------------------------+-------------------+
| Maximum Mental          | ``Willpower * 5 + Intelligence * 2 + Wisdom``| Species, Traits,  |
| Sustainable Fatigue     |                                              | Buffs             |
+-------------------------+----------------------------------------------+-------------------+
| Physical Recovery       | 5 physical fatigue points per round          | Species, Traits,  |
| Rate                    |                                              | Buffs             |
+-------------------------+----------------------------------------------+-------------------+
| Mental Recovery         | 5 mental fatigue points per round            | Species, Traits,  |
| Rate                    |                                              | Buffs             |
+-------------------------+----------------------------------------------+-------------------+


A character having accumulated too much mental fatigue may be:

 - *Stunned*: the character is unable to act during a short duration

 - *Paralyzed*: the character is completely unresponsive (no action, perception, thinking), and it may last

 - *Poisoned*: the character suffers from a loss of health per unit of time, which fades away slowly

 - *Acid-attacked*: the character suffers from a significant loss of health per unit of time, which fades away relatively quickly

 - more likely affected by negative effects of altered states (*Cursed/Slowed/Charmed/Scared*)


Species Fatigue Modifiers
.........................

+------------------+-------+-----+-------+----------+-------+--------+-----+
| Species          | Human | Elf | Dwarf | Halfling | Gnome | Goblin | Orc |
| Resistance       |       |     |       |          |       |        |     |
| To Fatigue       |       |     |       |          |       |        |     |
+==================+=======+=====+=======+==========+=======+========+=====+
| Physical Fatigue | +0%   | -5% | +10%  | -5%      | -10%  | +5%    | +12%|
+------------------+-------+-----+-------+----------+-------+--------+-----+
| Mental Fatigue   | +0%   | +5% | +25%  | -10%     | +5%   | +5%    | -8% |
+------------------+-------+-----+-------+----------+-------+--------+-----+



Fatigue Due To Carried Weight
.............................

Knowing that ``SWF`` is the ``Species Weight Factor`` and ``NCW`` is the character ``Nominal Carried Weight``:

 - a static (non-moving character) can carry up to ``SWF * NCW`` kilograms
 - a walking character can carry up to ``NCW`` kilograms without additional fatigue penalty
 - a running character can carry up to ``NCW/4`` kilograms without additional fatigue penalty


Let's take the example of Ulf the Gnome, whose ``NCW`` is equal to 4 (it is a strong gnome indeed). As a gnome, Ulf has a ``SWF`` of 2.

Thus:

 - Ulf can carry without moving no more than ``SWF * NCW = 8 kilograms``
 - he can walk with up to ``NCW = 4 kilograms`` without additional fatigue




Fatigue Due To Environment
..........................

Specific environments may result in additional fatigue modifiers. This is the case when a character is affected by gravity forces that are not the ones of this native environment (ex: jumping while on the Moon implies less fatigue than doing the same on Earth), or when operating under difficult conditions (humidity, temperature, etc.).


Fatigue Due To Actions
......................

Each action (ex: wielding a sword, climbing a wall, casting a spell, etc.) incurs fatigue.



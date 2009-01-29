
:raw-latex:`\pagebreak`


A Focus on Conflicts & Combats
==============================


Combat Design Guidelines
------------------------

One concern is to avoid that experienced characters based on fighting skills (barbarians, sword masters, ninjas, etc.) become significantly weaker, limited and dull to play than magic users, notably in late game.

The solution used in Orge is to put the stress on the tactical combat, and to offer a wide range of actions for non-arcane users as well, thanks to various kinds of Close Combat (Unarmed, or with Melee or Pole Weapons) and Ranged Combat, and also thanks to Specialized Combat Technics, which includes many sorts of Defenses, Combat Maneuvers, Non Lethal Combat Technics, Battle Disciplines, Special Attacks, and Group Combat Technics.


Combats are expected to be finely driven by the players: for example, instead of just designating a target, the player is able to choose the weapon, the kind of attack, the attack/defense ratio, if not playing on a system allowing to accumulate power until unleashing it thanks to powerful blows. Of course spells can be cast, objects be used (ex: grenade-like).  

Combats seek a certain level of realism. For example, sword blows can be attempted even if no opponent is in range. In that case the attacking character just performs a fruitless movement.

But if an ally happen to be in the attack range, this unfortunate ally might be hurt: *friendly fire* can occur, since the game engine just simulates the attack and, based on its range, determines which creature(s) is/are affected, regardless of the intention of the attacker. In Orge like in the real world, only the acts matter.
  

Regarding time, a combat is made of a series of rounds, each round being further divided in a series of phases. As for all interactions, combat rounds are affected by `Time Bubbles`.


Combat Phases
-------------

A round is made of the following phases:

 #. initiative phase: the order in which creatures will act is determined
 #. action phase #1: during its turn, the first creature may perform any operations that are in its action budget
 #. defense phase #1: each actual target (explicit, or implicit as zone-based) that can defend itself will try to do so
 #.  action phase #2: the second creature acts
 #. [..]
 #. the outcomes for all parties are determined and applied, notably damages are dealt



Initiative phase
................


Here the action order of creatures is determined.

Under normal circumstances, all creatures are sorted by decreasing Quickness factor, and will start to act in that order.

The Quickness factor is based on the Quickness character attribute, modulated by the level L in the `Initiative Skill`_ and a floating-point random number R in [-1;+1]: ``Quickness factor = Quickness *(1+L/4+R)``.

In case of ties, order among creatures having equal Quickness factor will be ordered randomly.

There are special cases though, as any unaware creature will not be able to act the first round.

This includes:

 - creatures caught by surprise (ambush or accidental encounter)
 - creatures not able to pay attention: being mad, overwhelmed by fear, petrified, sleeping, knock-out, drunk or under the influence of narcotics or hypnotic drugs

Surprise is determined by the awareness of characters, based on their perception. No party, some or all parties can be surprized when encountering each other.

The initiative is evaluated each round, as various events can affect the creature ordering.



Action Phase
............


During its turn, as long as its action points allow it, a creature may perform any combination of following actions:

 - Move
 - Draw weapon
 - Aim
 - Improve Defense
 - Select Combat Manoeuver
 - Activate Battle Discipline
 - Attack
 	- Classical Attack
 	- Special Attack
 - Use Special Combat Technics
 	- Use Group Combat Technics
 	- Use Non Lethal Combat Technics
 - Charge   
 - Reload
 - Communicate
 - Take item from bag
 - Pick up item
 - Use item
 - Drop item (including weapon)
 - Put item in bag
 - Sheathe Weapon
 - Rest
 - Cast a spell


.. Note** A careful character should spare action and fatigue points during its action phase, in order to be still able to defend itself until its next action phase comes, since all its opponents will act inbetween.
 
 
Therefore the saved action points is a means of choosing the attack/defense ratio for a character. For instance, Bersekers are renown for having all-out attacks, and thus for almost never spending action points in defensive actions, in the hope their foe will be dead before being able to retaliate.
 
 
Action: Move
____________

Costs action points.



Action: Draw weapon
___________________


Costs action points.



Action: Aim
___________ 

Before attacking with a ranged weapon, it is advisable for a character to aim its target, in order to increase its chances of hitting:

+-------------+------------+--------------------+
| Aiming      | Aim        | Total To Hit Bonus |
| Duration    | Designation|                    |
| (in rounds) |            |                    |
+=============+============+====================+
| 1           | Aim        | +20%               |
+-------------+------------+--------------------+
| 2           | Best Aim   | +35%               |
+-------------+------------+--------------------+
| 3           | Better Aim | +45%               |
+-------------+------------+--------------------+

Of course, depending on the size and distance of the target (i.e. depending on its solid angle from the sniper), further modifiers will apply, if and only if the target is in range.

Some aiming can be performed with melee weapons too: a character might spend one round planning its melee attack for a +10% increase in its To Hit Bonus.

Costs action points.



Action: Improve Defense
_______________________ 

Spending action points to prepare defense allows the character to defend itself more effectively during its turn.


Costs action points.



Action: Select Combat Manoeuver
_______________________________ 


Costs action points.



Action: Activate Battle Discipline
__________________________________ 


Costs action points.



Action: Attack
_______________ 

The general process is, for each creature than happens to be targeted, to determine:

 #. its reaction (see the `Reactions` general section), quite often 
 #. whether the attack succeeded
 #. what are the consequences, including damages inflicted 
 
 
Classical Attack
****************

These can be either `Close Combat Attacks`_ or `Ranged Combat Attacks`_.
 
In both cases the opponent must be in range.



Special Attack
**************


Costs action points.



Action: Use Special Combat Technics
___________________________________ 


Use Group Combat Technics
*************************

Use Non Lethal Combat Technics
******************************


Costs action points.


Action: Charge
______________ 

Charging corresponds to running on a straight line to an opponent and engage an opponent in close-combat immediately afterwards.

Damages inflicted by the charging character will be increased of 20%, but its defense will be decreased of 25%. Opponent can be destabilized.

Costs action points.



Action: Reload
_______________ 


Costs action points.



Action: Communicate
___________________ 


Costs action points.



Action: Take item from bag
__________________________

Costs action points.



Action: Pick up item
____________________

The item will be taken from the ground and then held in hand.

Costs action points.



Action: Use item
________________ 

This item must be already held in hand.
This includes lighting a lamp, quaffing a potion, blowing a horn, etc.


Costs action points.



Action: Drop item
_________________ 

The held item will be dropped on the ground.

Includes weapons.

Costs action points.



Action: Put item in bag
_______________________

A held item will be put in bag (if any and if having enough room).

Costs action points, whether it succeeds or not.



Action: Sheathe Weapon
______________________

A weapon (ex: a sword) will be put back in any protective container (ex: case), in an adequate way to free the character's hands (ex: for easier travelling).

Costs action points.



Action: Rest
____________ 

The character does not do anything, but this will cost no action point this round, and the character will be relieved a bit from its physical fatigue.

Costs no action point.



Action: Cast spell
__________________ 


Costs action points.




.. comment Unarmed combat: punch, kick, bite, gouge, butt, grapple, tackle, shove, circle, weave, choke




Defense Phase
.............


In close-combat, a character can try to:

  - dodge an attack
  - parry the attack if using an appropriate weapon
  - disengage


Species To-Hit Modifiers
________________________


Depending on the species, a creature may or may be harder to hit in close combat. A positive *To-Hit Modifier* means that the corresponding creature will be easier to hit.

This modifier depends mostly on the shape and size of the species. For example, Gnomes, thanks to their tiny sizes, are a real nightmare to impale.


+----------------+-------+-----+-------+----------+-------+--------+-----+
| Species To-Hit | Human | Elf | Dwarf | Halfling | Gnome | Goblin | Orc |
| Modifiers      |       |     |       |          |       |        |     |
+================+=======+=====+=======+==========+=======+========+=====+
| Slashing       | +4%   | +2% | +8%   | -5%      | -25%  | -5%    | +5% |
+----------------+-------+-----+-------+----------+-------+--------+-----+
| Piercing       | +4%   | +2% | +8%   | -5%      | -35%  | -5%    | +5% |
+----------------+-------+-----+-------+----------+-------+--------+-----+
| Bludgeoning    | +5%   | +5% | +10%  | -5%      | -30%  | +0%    | +8% |
+----------------+-------+-----+-------+----------+-------+--------+-----+
| Cold           | +4%   | +2% | +2%   | +0%      | -15%  | -5%    | +5% |
+----------------+-------+-----+-------+----------+-------+--------+-----+
| Fire           | +4%   | +2% | +8%   | -5%      | -15%  | -5%    | +0% |
+----------------+-------+-----+-------+----------+-------+--------+-----+
| Electrical     | +4%   | +2% | +10%  | -5%      | -25%  | -5%    | +8% |
| & Lightning    |       |     |       |          |       |        |     |
+----------------+-------+-----+-------+----------+-------+--------+-----+
| Arcane         | +0%   | +4% | +4%   | -5%      | -20%  | -5%    | +5% |
+----------------+-------+-----+-------+----------+-------+--------+-----+
| Poison         | +4%   | +2% | +8%   | -5%      | -25%  | -5%    | +5% |
+----------------+-------+-----+-------+----------+-------+--------+-----+
| Acid           | +4%   | +2% | +8%   | -5%      | -25%  | -5%    | +5% |
+----------------+-------+-----+-------+----------+-------+--------+-----+
| Sonic          | +0%   | +0% | +0%   | +0%      | +0%   | +0%    | +0% |
+----------------+-------+-----+-------+----------+-------+--------+-----+


Depending on the environment and on the character's position, defensive bonus due to cover shall be applied. For example, if squatting being sand bags, a character should be harder to hit by ranged weapons.






The Attack Rate depends on:

 - the weapon used
 - the physical fatigue of the attacker
 - the Quickness of the attacker


When a range weapon is fired (ex: a bow throws an arrow), the first element in the trajectory of the missile will be hit. There is therefore a risk of friendly fire, and an advantage at being sheltered by scenery elements.

 
The Hit Bonus depends on:

 - when attacking with Hand-to-hand, Melee or Pole Weapons: first Strength, then Agility, modulated by mental fatigue
 - when attacking with Ranged Weapons: first Agility, then Strength, modulated by mental fatigue


Close Combat Attacks
--------------------

Depending on the melee weapon being used, a character might use one or more of the supported attack styles (see Weapons_).

For example, if using a sword, the character will be able to try to slash or to pierce its opponent.

This directly impacts the probability to hit and the inflicted damages.

Currently no specific body part of the opponent can be targeted when striking.





Ranged Combat Attacks
---------------------

.. Note:: The game system has to choose between following the exact missile trajectory and determining what it hits, or only calculating modifiers to hit the specified target, depending on its distance, range, cover, etc. Orge uses currently this latter simplier approach, thus friendly fire cannot occur.


.. comment The missile moves from the attacker in a given direction until either it hits a target or it reaches its maximum range.
.. Here we must determine whether a target is hit and, if yes, which it is.



Determining whether the Target is Hit
.....................................


First step is to check whether the target is in range. If not, the attack fails directly.

Otherwise, the to-hit modifier has to be determined.

All ranged combat attacks are based on solid angles.

We define here the solid angle A of a body B from a viewpoint V as: ``A = 25 * Surface(B)/(Pi.Distance(B,V)^2)``, with ``Surface(B)`` being the surface of B seen from V and ``Distance(B,V)`` being the distance between B and V. A is directly the percentage (ranging from 0..100) of the sphere (i.e., fractional area).

.. comment 25 = 100 /4 (4 for the sphere area formula, 100 to have a percentage)

For example, a character trapped in a sphere would see it with an solid angle of 100. A shape whose area would be 1m², located at 2m from the viewpoint, would represent a solid angle of ``A = 25*1/(Pi*2^2) = 2``.


We define the Neutral Solid Angle (NSA) as the solid angle that leads to neither a to-hit bonus nor a to-hit malus for an average man. We take here ``NSA=0.08``, which corresponds roughly to a surface of 1m² at a distance of 10m. We expect the average man to have as many chances to miss than hit that target with a standard bow.

.. solid angles range from 0 to +infinity, clamped to 100. Modifiers range from -100% to +100%, and for solid angle NSA, modifier is 0%.


The following to-hit modifiers abacus allows to transform a solid angle into a to-hit modifier:

nn nn:raw-html:`<img src="solid-angle-modifier-negated.png"></img>`
nn nn:raw-latex:`\includegraphics[scale=0.75]{solid-angle-modifier.png}`

Some computed examples are::

  Surface = 1 m^2, distance = 1 m  -> angle =  7.96, to-hit modifier = 100.0 %.
  Surface = 1 m^2, distance = 2 m  -> angle =  1.99, to-hit modifier = 85.2 %.
  Surface = 1 m^2, distance = 5 m  -> angle =  0.32, to-hit modifier = 21.2 %.
  Surface = 1 m^2, distance = 10 m -> angle =  0.08, to-hit modifier = -0.4 %.
  Surface = 1 m^2, distance = 15 m -> angle =  0.04, to-hit modifier = -49.4 %.
  Surface = 1 m^2, distance = 20 m -> angle =  0.02, to-hit modifier = -71.8 %.
  Surface = 1 m^2, distance = 25 m -> angle =  0.01, to-hit modifier = -83.2 %.
  Surface = 1 m^2, distance = 30 m -> angle =  0.01, to-hit modifier = -89.7 %.
  Surface = 1 m^2, distance = 40 m -> angle =  0.00, to-hit modifier = -96.4 %.
  Surface = 1 m^2, distance = 50 m -> angle =  0.00, to-hit modifier = -99.6 %.

  Surface = 2 m^2, distance = 1 m  -> angle = 15.92, to-hit modifier = 100.0 %.
  Surface = 2 m^2, distance = 2 m  -> angle =  3.98, to-hit modifier = 98.0 %.
  Surface = 2 m^2, distance = 5 m  -> angle =  0.64, to-hit modifier = 42.7 %.
  Surface = 2 m^2, distance = 10 m -> angle =  0.16, to-hit modifier = 7.6 %.
  Surface = 2 m^2, distance = 15 m -> angle =  0.07, to-hit modifier = -8.7 %.
  Surface = 2 m^2, distance = 20 m -> angle =  0.04, to-hit modifier = -43.6 %.
  Surface = 2 m^2, distance = 25 m -> angle =  0.03, to-hit modifier = -63.4 %.
  Surface = 2 m^2, distance = 30 m -> angle =  0.02, to-hit modifier = -75.2 %.
  Surface = 2 m^2, distance = 40 m -> angle =  0.01, to-hit modifier = -87.9 %.
  Surface = 2 m^2, distance = 50 m -> angle =  0.01, to-hit modifier = -94.0 %.


For example, the same target area of 1m², when being twice closer to the sniper than for the NSA (i.e. 5m instead of 10m), results in a solid angle of ``A = 25/(Pi*4) = 2``, thus on a to-hit modifier of .



The maximum range of a missile is determined from the kind of missile (ex: war arrows) and, for thrown missiles, also from the strength of the thrower.

Damages inflicted by a missile depend on the fraction of the maximum range that had to be covered by the missile before hitting its target: 

+------------------+-----------------+
| Range Percentage | Damage Modifier |
| Being Covered    |                 |
+==================+=================+
| 0-20%            | +25%            |
+------------------+-----------------+
| 20-40%           | +5%             |
+------------------+-----------------+
| 40-60%           | -10%            |
+------------------+-----------------+
| 60-80%           | -40%            |
+------------------+-----------------+
| 80-100%          | -80%            |
+------------------+-----------------+



Defenses
--------

Defenses are `Reactions`_ to attacks.

In general, to an attack several reactions could be chosen, in:

 - doing nothing
 - dodging
 - parrying
 - blocking
 - resisting spell
 

Defensive actions come at a cost for a character, spent in action and fatigue points. Otherwise a character could be attacked by any number of opponents without suffering from penalties.

The Defense Bonus depends on first Agility, then Quickness, for all weapons (Hand-to-hand, Melee, Pole and Ranged Weapons).

It may fail or succeed.

.. comment add critical strikes/critical failures



Attack Outcomes
---------------

If a defensive action failed, or if the defender does nothing (either by choice or by constraint), then the attack succeeded and its effects are to take place.

Depending on the attacker weapon and the defender protection, it may or may not be hurt.

Note that a creature being unable to defend at all (ex: being unconscious or attacked during sleep) will incur tremendous damages that will be likely to be fatal.

The protection results from the character itself (its own damage resistances) and from its armor (inducing damage resistance modifiers).

An attack - successful or not - is resolved utimately to changes in:

 - physical and mental fatigue, experienced by the attacker and the defender
 
 - skill progress, experienced by both as well (ex: chop for the attacker, parry for the defender)
 
 - hit points, experienced by both as well (ex: defender is hurt, and attacker drained life or performed a critical failure)

See also: `Damage Resistance`_.



Progression System 
------------------

A powerful motivation for RPG players is to make their character progress. The usual scheme implies that the player is rewarded by a permanent increase in the abilities of his character after enough successful actions, which will be detailed in `Causes of Experience Gains`_.

Usually a character starts a game unexperienced, young, at least partially untrained. Generally the lower the better, since it increases the character room for progression and, therefore, the game lifespan and the player pleasure (often more or less proportional to his character development).

Thus many CRPG are based on a training stage/tutorial involving (in the best cases for the player) a butchery of rats.

For long-lived games, such as MMORPG, the character progression is ideally never-ending, otherwise players grow tired of the game.

Therefore a crucial point is to find the subtle balance between frustation (if too little reward is granted) and too quick progression (too easy and leading to too powerful unbalanced characters).



measure of character development
`experience points <http://en.wikipedia.org/wiki/Experience_points>`_ 


.. Note:: Adventures are generally short compared to a character lifespan: a great deal of his experience should come from the time he spends living (and training) between adventures. Players should be able to define what their characters do (ex: farming, training, etc.) between game sessions, when there is no player controlling them.  



Levels considered harmful?
..........................

Speaking of realism, levels should be avoided due to the threshold effect they induce: instead of a continuous progress, levelling up results in sharp rises of the potential of a character that corresponds to nothing in the real life.

We chose however to keep a level-based system for creatures. Why? One might call it nostalgia of old-school games, but the actual motivation of that choice is *player  pleasure*. Most players love when their character gets stronger, and if there is no particular event that underlines it, they feel frustated.

A bit like the epic nature of stories, with Orge the trade-off between realism and entertainment has been geared towards the former instead of the latter. 

A rule of thumb regarding experience is that:
 
 - the more powerful a character is, the slower and more difficult its progression should be
 
 - rewards should be expressed independently from the level of the rewarded (no tweaking of their value)

With Orge we chose:

  - to start from level 0, which corresponds to average people (John Doe is level 0)

  - to define fixed amounts of experience gain in case of success in specific actions, whereas the experience needed to level up increases quickly with the target level

  - not to limit the maximum level of a character



This is done according to following formula:



It leads to following progression:

..  ddd:raw-html:`<img src="experience-for-level-negated.png"></img>`
..   ggg:raw-latex:`\includegraphics[scale=0.75]{experience-for-level.png}`


For first levels, this table defines these experience thresholds:

============ ============================================
Target Level Experience Points Needed to reach that level
============ ============================================
0            0
1            125
2            216
3            343
4            512
5            729
6            1000
7            1331
8            1728
9            2197
10           2744
11           3375
12           4096
13           4913
14           5832
15           6859
16           8000
17           9261
18           10648
19           12167
20           13824
============ ============================================


.. Note:: The experience gap between levels grows indeed fast, but it is partially offset by granted rewards, that become - generally - higher.




Causes of Experience Gains
..........................

The convention here is that experience can only increase over time, except some very rare story events. 

Generally speaking, experience is here to reward good roleplay by the player, by increasing the abilities of his character.

Following actions results in an experience increase:

 #. increasing skills
 #. fulfilling quests
 #. slaughtering monsters
 #. finding rare objects
 #. solving an enigma, a challenge, a puzzle

The model we chose is that associated with each of these events, a experience bonus is defined (its value is to be chosen by the Scenario Writer). 

Regarding the experience gained after a success in combat in a multi-character context, to avoid that only the character striking the deathblow gets  experience points from the battle (which may lead to a severely unbalanced party), each character will receive experience proportionnaly to the amount of loss of life it inflicted, albeit the deathblow dealer will have a small bonus (20% of the total).

So if the victory over a given opponent is to be rewarded by N experience points, they will dispatched that way:

 - the deathblow dealer will receive a rounded ``0.2*N`` experience bonus
 - an attacker (including deathblow dealer) who inflicted a percentage P in [0..1] of the actual damage will receive ``P*0.8*N``

The damage are taken into account accross the full lifespan of the killed creature, and experience points assigned to attackers being dead between their strike and the death of the creature will be lost.
  


Consequences of Experience Gains
........................


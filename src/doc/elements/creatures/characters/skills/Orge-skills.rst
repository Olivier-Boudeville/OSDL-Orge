Skills
------ 

.. contents:: 
	:local:
	:depth: 1



Skill Definition
................

A skill defines the capacity of a character to perform a specific kind of action, regarding the technics, the know-how, the knowledge. 

For example, for a successful horse-riding, following needs can be identified:
  
  - the character must be strong and agile enough
  - he should be concentrated enough on his task
  - he should have enough practise
  
The first two points are dictated mostly by the character attributes (ex: strength, agility, willpower), whereas the last one corresponds to a know-how, the horse-riding skill. 

Only the know-how that cannot be expressed by roleplay should be captured by skills. That is the case of practical knowledge like horse-riding, or the ones that cannot be translated in-game, like mastery of an oral Orc idiom. 

Conversely, philosophic knowledge or debating should not be character skills, as they depend more on the player than on her character: if a player were a good debater and her character a poor one according to its attributes, the overall result would be still that the character behaves apparently like a good debater. 

A corollary is that even if the game system ensures that initially all characters are roughly equal regarding statistics, due to the players this cannot be true in-game anymore. This can be seen as good character direction being rewarded by the game system.



Specific Skill Level
....................


The extent to which a character masters a specific kind of action as such is modelled by a ``Specific Skill Level``.

More precisely, the current level of practise of a character in a given skill is described as a positive integer level. Regarding a skill, unexperienced characters start at level 0 (``L0``), whereas it is not uncommon to encounter specialized characters at level 5 or higher in carefully selected skills.

This *Specific Skill Level* takes into account only the training in that particular skill: the effect of more generic skills that could reinforce this target skill will be integrated later, in the *Actual Skill Level*.




Skill Tree
..........


Specialization Of Skills
________________________


Skills are sorted hierarchically, in the *Skill Tree*, presented below. The reasons for that sorting is that most skills can be refined again and again in more specialized skills.

For instance, if a character has a strong horse-riding skill, then she should be quite good at warhorse-riding, but nevertheless weaker than the same character who would have invested most of its efforts directly in warhorse-riding.

On the other hand, the first character should be able to ride more effectively all kinds of horses than the second one. This is the usual trade-off between genericity and efficiency.

.. commented A skill is said to be *terminal* if it cannot be further specialized (i.e. it is a leaf of the Skill Tree).



Skill Development
_________________


Some rules apply to skill development:

 - for each skill, a ``Basic Practise Level`` (BPL) is defined. This is the minimum *Specific Skill Level* that must be obtained before being able to develop any specialized skill of that skill. For example, if the BPL for horse-riding is 4, a character cannot be trained to warhorse-riding until it is at least horse-riding level 4  
 
 - then, once the BPL is reached, further investments in that parent skill are still valuable, as they will result in increasing all the direct specialized skills underneath, according to the following recursive [#]_ law: the ``Actual Skill Level`` for any given skill is equal to the current level of that skill, augmented by: the current actual level of the *parent* skill, once divided by the number of children of that parent skill. The *Actual Skill Level* is thus a (positive) fractional value

.. [#] The law is recursive because a parent skill might itself have an increased actual level thanks to its own parent skill, etc. Thus the whole skill tree has to be evaluated from root to leaves to determine the actual skill levels.


For instance, let's suppose the horse-riding skill has three specialized skills: War-horse, Race-horse and Unicorn riding. We want to know here the ``Actual Skill Level`` of Helena The Amazon. 

Helena has a *Specific Skill Level* regarding *warhorse*-riding of 2, and a *Actual Skill Level* regarding *horse*-riding of 4.2. Note it is already a fraction, as the horse-riding parent skill, mount-riding, brought already some bonus to its child skill, including horse-riding. Note also that, since Helena was able to train warhorse-riding, this implies she reached already the horse-riding BPL, and, a fortiori, the mount-riding BPL.

What is the ``Actual Skill Level`` of Helena regarding warhorse-riding ? It is equal to the corresponding *Specific Skill Level* (2) plus the parent *Actual Skill Level* (4.2) divided by the number of its specialized skills (War-horse, Race-horse and Unicorn riding, thus 3). Therefore the warhorse-riding actual skill level for Helena is ``2+4.2/3=3.4``.

.. Note also there is a slight advantage in developing the most generic skills   
.. comment an inheritance coefficient (ex: 2/3) could be used to decrease the interest on investing only in top skills? Note most progression comes from skill practise, i.e. it is seldom a question of selecting which skill level should be increased. 
 
A corollary is that specialized skills of a skill are not taken into account when computing the level of that skill: only the parent skills matter.

 
  

The Orge Skill Tree is the following:

..  ddd:raw-html:`<img src="skill-tree-negated.png"></img>`
..  ggg:raw-latex:`\includegraphics[scale=0.75]{skill-tree.png}`


.. include:: Orge-skill-list.rst



Skill Consequences
..................


Skills & Challenge Outcomes
___________________________


Skills would be useless, had they no impact on the game flow. They actually play a major role each time a character has to overcome a *challenge*.

Challenges may be to ride a mustang, to climb a wall, to play the violin, or to throw a poisoned dart between two pieces of armor of a foe.

A challenge will be resolved depending on numerous parameters, including the character statistics (ex: strength, dexterity, etc.) and state (drunk, wounded, sleepy, etc.), the characteristics of the context (ex: the opponent, the weather, the daylight, etc.).

All these parameters are taken into account thanks to modifiers, that will lower or raise the probability of success of the character in the challenge. The ``Skill Action Modifier``, directly proportional to the character's Skill Level, is one of these parameters.


Skill Action Modifier
_____________________


Dependence of an Action against a Skill
***************************************

Some actions depend heavily on skills (ex: try to play the violin without having been trained accordingly!), others almost not (ex: arm-wrestling contests are mostly strength challenges where skills matter only a little).

This is modelled by associating to each skill a ``Skill Action Modifier Factor`` (SAMF), which is a skill-specific integer multiplicative factor, usually in the 1..15 range, that will be multiplied by the Skill Level of the character to obtain the Skill Modifier that will be applied to the character's actions involving that skill. The more a skill matters for an activity, the higher its Skill Action Modifier Factor will be.

For example, playing the violin cannot be achieved without the adequate training, therefore the corresponding ``Skill Action Modifier Factor`` is very high: 20. This means a level 3 violin player will have a Skill Action Modifier of ``3*20=+60%`` that will be applied to her probability of succeeding in playing the violin - due to her skill (knowing that many more contextual modifiers can apply).  


.. Note:: The SAMF is to be multiplied by the *actual* skill level of the character (*Actual Skill Level*, not *Specific Skill Level*), i.e. once all the effects of more generic skills have been taken into account. 


Action Performed By Multiple Characters
***************************************

When the overall skill of a party must be determined from the skill of each of its members, one of these three modes will be chosen, depending on the action performed:

 - ``most skilled member``: the group as a whole will behave as one character whose skill level is the one of the character who is the best in that skill. For example, when lockpicking, only the best character at it is expected to make an attempt

 - ``least skilled member``: the group as a whole will behave as one character whose skill level is the one of the character who is the worst in that skill. For example, when attempting a group stealth operation or a fast group move, only the worst character is expected to matter
 
 - ``mean skilled member``: the group as a whole will behave as one character whose skill level is the mean of all skill levels in that group. For example, when the party is singing along, the choir will behave as if being homogeneous in that singing skill
 



Skill Examples
______________


In basic situations (ex: running for your life), which depend directly on a corresponding skill (ex: the sprint skill), as we have seen the relevant modifier is equal to the level of expertise of the character in that skill, times the Skill Modifier Factor (ex: 4 for the sprint skill). Thus, regarding that skill, a level 2 sprinter should benefit from a ``2*4=+8%`` modifier.

The outcome of more complex actions can be modelled as depending on a weighted list of skills. 


Let's say William The Bard, during a feast where too much wine was poured, is trying to steal a rolled map still lying on the counsellor's table while entertaining the mixed audience with a melodious theme on his violin. Even if most of the public is quite drunk and their attention is drawn to other entertaining shows, William's objective is still a tough challenge, which involves, among other parameters, two skills: the violin mastery (our Bard is level 4) and the stealthy robbery (William has limited experience in that discipline, i.e. he is only a level 1 pick-pocket).  
 
The guests being drunk, and the violin interpretation involving both hands of Williams (grabbing the map will be harder), violin mastery might be deemed by the Game Master less important than the pick-pocket skill in that occasion. So he may choose that the pick-pocket skill matters three times as much as the violin mastery for that action.

The overall Skill Modifier results therefore from a weighted sum (actually a barycenter): it is equal to ``(3*SkillModifier(Pickpocket)+1*SkillModifier(Violin))/(3+1)``.

Knowing that the Skill Modifier Factor for the pick-pocket skill is 11, the overall Skill Modifier for William will be ``(3*1*11+1*4*20)/4 = +29%``. This is quite a huge modifier, but the base probability of success for this action would be most probably very low, as the task is far from trivial (notably if not knowing how to play the violin).







Skill Progression
.................


Progression Principle
_____________________


Practise & Training
*******************

A skill can be improved thanks to practise and, less usually, thanks to appropriate training, with a mentor and/or books.
 
Regarding practise, an action generally leads to a slight increase in the associated skill(s), whether the action succeeds or not. However the skill gain is usually smaller if the action failed.

This models the fact that, for most actions, the more one practises, the better at it one becomes, knowing that even failures teach lessons.

Gained skill points depend also on the intrinsic difficulty of that particular challenge: playing a few notes is less tricky than playing a full-blown sonata, stabbing a Lich King is more difficult than cutting a slice of bread. In each case, the first outcome should be more rewarded than the second one.

Note that the discussion is based on whether a challenge is difficult or not, not on whether succeeding can be interesting or not.

See also: `Age vs Skills Trade-Off`_.


Active vs Idle Training
***********************

Finally, a player does not have always to perform *active training*, i.e. he does not have to make its character practise himself.

Indeed the Orge system will take into account durations between player connections to that player account, to figure out how much a character succeeded to train inbetween. This is called *idle training*.

Note that skills to be developed should be specified before the disconnection, otherwise the character, without hint, will develop its skills at random. 

Note also that idle training is four times less time-effective than active training. The main reason is that idle training does not involve any risk.



Actual Modelling of the Progress In a Skill
___________________________________________


Skills are threshold-based (due to the skill levels) and follow a continuous, almost invisible, progression, based on the accumulation of skill points.



Dispatching of Skill Points
***************************

More precisely, each action, when performed, will increase the skill points of the aforementioned list of skills that are involved for that action (the list can be empty), proportionally to their respective weight.

For example, we saw that the Game Master determined for William that the success of his action was depending for 3/4 from the pick-pocket skill, and for 1/4 from the violin skill. Once the action will be performed, any reward in skill points - which will depend on the action outcome, success or failure - will be dispatched accordingly: 3/4 of the skill points will be added to the pick-pocket skill, 1/4 to the violin skill. Added skill points are always rounded down.   

William's objective is risky and involves quite a lot of talents. Here the Game Master determined that under normal circumstances, the (base) probability of success should be 15%. 

Regarding progression, each skill is caracterized by:

 - the number of skill points SPk needed to be gained, while at skill-level Lk, to reach Lk+1, for k greater or equal to zero. The required number of skill points grows according to a geometric series. This series is determined by its *Skill Initial Threshold*, ``SP0`` and its *Skill Progress Factor*, a floating-point multiplicative factor F,  constant for a given skill, greater or equal to one. Thus ``SPk+1 = SPk * F`` and ``SPk = SP0.F^k`` (rounded up). This implies that all skills will be more and more difficult to master, and that their progression rates can be set by the System Designer, on a per-skill basis. Improving one's violin skill is not a fast process: its SP0 is high and its significant progress factor F worsen the situation  
 - the skill factor which, applied to the Challenge Degree of Difficulty, determines how many skill points should be gained when an associated action:
 	
	#. succeeds: SSM, for *Skill Success Modifier*
	#. fails: SFM, for *Skill Failure Modifier*
	
In the vast majority of cases, ``SSM > SFM >= 0``. One should note that:

 - negative modifiers should preferably be avoided, to prevent the decrease in expertise, not wanted in this model
 
 - the skill points to be gained are absolute, intrinsic, i.e. they do not depend on the character, unlike systems where skill rewards are proportional to the difference of skill level between the character and an opponent
 
 - the SSM and SFM are only useful to distinguish the relative effects of success and failure, not to reflect how tough the challenge is (this is the role of the base probability of success an) 
 

To do so, the ``Challenge Difficulty Degree`` (CDD) must be defined. The CDD increases as the base probability of success (P, in the 0-1 range) decreases, according to this formula: ``CDD = exp(7x(1-P))-1``.

For example, for a nearly impossible action (P is zero), ``CDD = exp(7)-1 = 1095``, whereas for an action almost impossible to miss (P is one), ``CDD = 0``.

The reason for the 7 in the actual formula comes from ``CDD = exp(Mu.(1-P))-1`` where ``Mu = ln(CDD(0)+1)``. Thus if we want the reward for the success in an impossible action to be equal to, say, ``R = 1000`` (i.e. ``CDD(0) = R = 1000``), then this implies ``Mu = ln(1001)``, i.e. Mu roughly equal to 7.



   	

As already mentioned, beyond practise, training with a mentor and/or books is another way of improving a skill. A successful training session leads to one or more skills being increased of an amount of skill points.


Once a character, in level k for a skill, succeeds in accumulating SPk points for that skill, she is ready to level up to level k+1. This event will not occur immediately though: the character must have gone through a meditation phase beforehand. No more than one level per skill can be gained during any given meditation.

The reason behind the meditation constraint is that, in Orge, mental mechanisms have a strong impact on actual behaviours. And, beyond this roleplay-related reason, there is also a technical one: waiting for medidation allows to postpone skill threshold checkings. Therefore only the relevant skill modifiers (SSM/SFM) have to be applied during the normal course of events. Otherwise each action would lead to numerous computations, which would require quite of lot of rather useless processings. Finally, players are not expected to choose their skill upgrades during frantic combat, 

Progressing in a specialized skill implies progressing as well (but to a lesser extent) in its parent skills.

When a specialized skill levels up (i.e. reaches its SPk threshold), all ancester skills will be increased, the initial skill points being divided by three at each skill generation and rounded down: the direct parent of the skill leveling-up will receive SPk/3 skill points, its own parent SPk/9, etc. and ancester skills may level up as well.


Example of Skill Progression
____________________________


Let's take the example of Saskund the Short, a Halfling Rogue who is struggling to tame Buck, a wild poney whilst hanging on its back thanks to a providential saddle, freshly tied up to the poney.

Buck has a somewhat fierce temper, the corresponding Challenge Degree of Difficulty was set by the Game Master to 9. 

This action may be associated to a few skills, one of which is the aforementioned horse-riding skill (there is no poney-riding skill in Orge). Saskund had already his share of horse-riding during his childhood, so his current level of practise for that skill is L3 (had he been a total rookie, he would have been L0). 

This may suggest that he has a good probability of succeeding in staying on the saddle (depending on the context), but that's not the point we are studying here, which is: how his horse-riding skill will be affected, once the outcome of this action will been determined?

Here we consider that the horse-riding skill respects the usual law, parametrized this way: ``SP0= 100``, ``F = 1.4``, ``SSM = 4`` and ``SFM = 1``. Thus ``SPk = 100*(1.4)^k`` for horse-riding.

The current level of Saskund in that skill being L3, L4 will be reached when ``SP3 = 100*(1.4)^3 = 275`` skill points will have been gained from the moment L3 was obtained.

Fortunately for Saskund, past poney experiences granted him already 252 skill points since L3, and he succeeded this time again with this poney, so he gained ``Challenge Degree of Difficulty x SSM = 9 x 4 = 36`` skills points. This implies he just exceeded SP3, since ``252+36 = 288 >275``, hence his promotion to horse-riding level 4. As an additional consequence, ancester skills are increased: mount-riding gains ``SP3/3=91`` skills points, physical development gains ``SP3/9=30``etc.


Finally, if Saskund knew it [#]_, he would be now proud to be soon (i.e. once he will have meditated) horse-riding level 4 (L4), with already ``288 - 275 = 13`` skill points out of the ``SP4 = 385`` needed to reach level 5. Moreover, being horse-riding level 4 may be the remaining condition for Saskund to progress in its career path, from *Rogue* to *Cow-Boy*, fulfilling the dearest dreams of our poor lonesome Halfling.
 
	  

.. [#] There is no reason why a character could know precisely her actual numerical characteristics.


Implementation of Orge Skills
.............................


In Orge, each skill is described by an ``orge_skill`` record, whose fields are:


+-----------------------------+---------------------------------------------------------+--------------------+
| Name of the                 | Meaning                                                 | Unit & Constraints |
| Skill Attribute             |                                                         |                    |
+=============================+=========================================================+====================+
| ``skill_name``              | Unique name that identifies that skill (ex: 'climbing') | String             |
+-----------------------------+---------------------------------------------------------+--------------------+
| ``skill_description``       | Textual description of that skill                       | String             |
+-----------------------------+---------------------------------------------------------+--------------------+
| ``skill_modifier_factor``   | Multiplicative factor, used to convert a Skill Level    | Integer, usually   |
|                             | into a Skill Action Modifier                            | in the 1..15 range |
+-----------------------------+---------------------------------------------------------+--------------------+
| ``specialized_skills``      | List of all skills that are specializations of that     | List of Skill names|
|                             | skill. For instance, for the horse-riding skill, this   |                    |
|                             | could be: ``[racehorse-riding, warhorse-riding, ..]``   |                    |
+-----------------------------+---------------------------------------------------------+--------------------+
| ``skill_initial_threshold`` | Number of Skill Points needed to reach the first        | Number of Skill    |
|                             | level of practise (i.e. SP0, needed to go from L0 to L1)| Points             |
+-----------------------------+---------------------------------------------------------+--------------------+
| ``skill_progress_factor``   | Multiplicative factor, used to compute the number of    | Floating-point,    |
|                             | Skill Points needed for next skill level from current   | greater than 1     |
|                             | one (i.e. F, in ``SPk+1 = SPk * F``)                    |                    |
+-----------------------------+---------------------------------------------------------+--------------------+
| ``skill_success_modifier``  | Number of Skill Points that should be gained when an    | Number of Skill    |
|                             | associated action succeeds (i.e. SSM)                   | Points             |
+-----------------------------+---------------------------------------------------------+--------------------+
| ``skill_failure_modifier``  | Number of Skill Points that should be gained when an    | Number of Skill    |
|                             | associated action fails (i.e. SFM)                      | Points             |
+-----------------------------+---------------------------------------------------------+--------------------+



A creature state (in Orge, all characters, NPC, monsters, etc. are managed identically) records following skill informations:

 - for each skill, the character's skill level can be determined 
 
 
 

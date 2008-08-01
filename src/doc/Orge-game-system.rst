====
Orge
====

--------------------
OSDL RPG Game Engine
--------------------

.. role:: raw-html(raw)
   :format: html
   
.. role:: raw-latex(raw)
   :format: latex


:Author: Olivier Boudeville & Hirinkaël
:Contact: olivier.boudeville@esperide.com

:status: This is a work in progress
:copyright: 2008 Olivier Boudeville. Both this document and the game system it describes are released under a `Creative Commons <http://creativecommons.org>`_ license whose name is *Attribution-Noncommercial-Share Alike 3.0*. See `License for the game model (a.k.a. rules)`_ for further details.

:Dedication:

    For Orge users (scenario writers, game masters, players) & system co-designers.

:abstract:

    This document describes the **OSDL RPG Game Engine**, which is a role-playing game system, mainly dedicated to video games in an heroic-fantasy setting. It could be easily adapted as well for table-top playing and for others settings.
	
	**Orge** is a game system that tries to focus on generic yet lightweight guidelines and rules, by minimizing the number of concepts and associating them in a logical manner. It includes a model (a set of simulation rules) and its corresponding implementation. 

.. meta::
   :keywords: game system, role-playing, RPG, engine, video game, OSDL, Orge
   :description lang=en: A description of the Orge game system, containing examples of all basic rules and many advanced ones.


.. contents:: Table of Contents
	:depth: 2

.. section-numbering::


.. include:: Orge-implementation-references.rst


:raw-latex:`\pagebreak`

Overview
========

Orge, the **OSDL RPG Game Engine**, is a set of game rules to be used in role-playing games (RPG). 

Here we designate by *RPG Game Engine* the set of high-level rules that are used to simulate a virtual world, manually (table-top games with a real Game Master) or, more importantly here, with the help of a computer (notably in the case of a video game, with an emulated Game Master).

Concerns like rendering, input handling, etc. are deemed outside of the scope of the game engine: if the game was a networked one, we would focus here on the server-side simulation of the game actors only.

Our goal is to define here:

 - what are the general requirements of most of the Role-Playing Games we aim at, regarding the underlying game engine
 - what is the solution we preferred to apply in order to fulfill these needs, and why
 - how to use that solution (Orge) in practise





Some definitions first
----------------------

These following central terms will be used everywhere in this document.
See also the `Orge Glossary`_.


Types of games
..............

Table-top Game
	This is the classical `role-playing game <http://en.wikipedia.org/wiki/Role-playing_game>`_, also known as *pen and paper* game, where participants meet in real life with little or no technical help (only documentation, dice, a cardboard screen, etc.).   

RPG Video Game, or Computer role-playing game (`CRPG <http://en.wikipedia.org/wiki/Computer_role-playing_game>`_)
	This was, originally, mostly a translation attempt of table-top games into a video game, even if it resulted in a rather different game genre: roleplay disappeared a lot and let its place too often to `levelling <http://en.wikipedia.org/wiki/Level_up#Level-based_progression>`_, with simplistic scenarios involving stereotypical scenarios (door/monster/loot, repeat).
	



Game Roles 
..........


System Designer (SD)
	This is the author of the Game System, i.e. of the generic conventions and rules that can be applied in all the kinds of supported games. Typically here the SD is us, the Orge authors, writers of the document you are reading. 

Scenario Writer (SW)
	This is the demiurge and story teller, who defines the world in which the action will take place, with its associated plots, quests, characters, etc. He will rely on the Game System provided by the SD to establish the basic rules of interaction, so that he can focus on the core of its subjects, the story. Note that the Scenario Writer defines a story only once, but it can result in any number of game instances based on that story.

Game Master (GM)
	This is the one that allows the players to interact with the game world, in the context of a story. His role is to animate a game instance based on a story created by a SW. 

Player 
	The player is interpreting the role of a character involved in the world recreated by the GM (*roleplay*). 


Of course one can cumulate multiple game roles: for example some GM prefer pre-cooked scenario, whereas others are SW as well. 	

The link between SW and GM exists for CRPG too, as the tools for scenario creation (editors for characters, objects, quests, maps, etc.) must be compatible with the actual RPG engine.

	
	
Characters
..........

		
Character
	Generally speaking, a Character corresponds to a sentient being. The definition has to be rather vague, as, beyond the classical men or women, a Character could be as well an alien or an advanced computer endowed with reason. Creatures deemed not smart enough to have elaborate feelings are not characters, they are sometimes called monsters.   
	
Player Character (PC)
	A PC is a Character that is interpreted by a player, who assumes his role and take control over many of his actions
	
Non-Player Character (NPC)
	A NPC is a Character that is not interpreted by a player, it has thus to be managed by the GM




Other Conventions
.................


Terms
_____


Buff
	Temporary beneficial effect on a character. See also this `Wikipedia article <http://en.wikipedia.org/wiki/Buff_(computer_gaming)>`_. In the context of Orge, a Buff can come from two sources: drugs and magic
	
iff
	It means `If And Only If <http://en.wikipedia.org/wiki/If_and_only_if>`_. ``A iff B`` means either both statements are true or both are false. 

N/A
	It means ``Non Applicable``. 

Party
	A group of characters, often complementary, working as a team.
	
Singleton
	A character who does not belong to a party, adventuring on its own.
		

Units & Orders of Magnitude
___________________________

Units are to be expressed mostly in the `international system (SI) of units <http://en.wikipedia.org/wiki/SI_base_unit>`_ and its `derived units <http://en.wikipedia.org/wiki/SI_derived_unit>`_. 

This includes:

================== ====== =================== =============================== 
Unit name          Symbol Associated Quantity Common multiples
================== ====== =================== =============================== 
metre 	           m      length              kilometer (km), centimeter (cm)
cubic decimetre    dm^3   volume              cubic metre (1 m^3 = 1000 dm^3), litre (1L = 1 dm^3) 
kilogram           kg     mass                gram (1 kg = 1000 g), metric ton (1t = 1000 kg)
second 	           s      time                minute, hour, day, year, century
metre per second   m/s    speed               km/h (1 m/s = 3.6 km/h)
degree Celsius     °C     temperature         (none)
newton 	           N      force               kilonewtons (1 kN = 1 000 N)
joule              J      energy, heat        kilojoule (1 kJ = 1 000 J)
candela	           cd     luminous intensity  (none)
pascal             Pa     pressure            kilopascal (1 kPa = 1000 Pa)
================== ====== =================== =============================== 


To better evaluate physicalo values, please refer to the examples listed in `Some scales and orders of magnitude`_.



Wrapping around

Usually there is exactly one Game Master, a few players, each interpreting only one particular Player Character, and a large number of beings, including monsters and NPC.

The game session may or may be split into sessions.

In the case of table-top games, a *game* follows a story line made usually of a set of *campaigns*, which are made of a series of *adventures*, which, for convenience reasons (due to duration above a few hours) are broken into a series of *game sessions*.

For RPG video games, either the world instance is created especially for a group of players, and then the world exist only while they are playing (be it a single player or multiplayer game), or the world instance lives continuously, i.e. is persistent be there players or not, in the case of a `MMORPG <http://en.wikipedia.org/wiki/MMORPG>`_ (*Massively Multiplayer Online Role-Playing Game*).

In both table-top and video games, an adventure includes generally a set of *quests*, or missions.

All participants are expected to create and follow *collaboratively* a story based on roleplaying.




Operating Principle
-------------------

The role of a game engine like Orge is mostly to *emulate* a Game Master, during a game session with real (human) player(s). 

The task includes:

 - describing to the players the world surrounding their characters
 - offering the players the means to interpret their character
 - resolving the corresponding actions 
 - evaluating the state and behaviour of all game elements besides the ones controlled by the players


The GM task can be described also as an interactive simulator operating on game elements chosen, defined and arranged by the SW, based on the modelling provided by the SD. Thus the vocabulary of simulation is often used in this document. Games are just a specific kind of simulation indeed.

.. Note:: Although there are presumably only a few CRPG (actually we do not know  any of such games) that offer to a human player the technical possibility of being the GM instead of controlling a character, this possibility could be interesting and thus could be supported in the future by the Orge implementation.  

Emulating the GM corresponds to the server-side computations of a game: we are dealing here only with models, without taking account input management, rendering nor other concerns. This is the place where we evaluate the game world and update its state (based on previous states and all inputs). 

Thus, for a MMORPG, Orge is to be found running on the datacenter hosted by the game operator. For a multiplayer game, Orge would be running on the game server(s). For a single player game, Orge could be run on the computer of the player. It would be still running logically as a server (even though technicall it could be in the same process with no network I/O).



Video Games versus Table-top Games
----------------------------------

Orge is a game system primarily dedicated to video games. 

However it can be used as well for table-top games, since most computations remain simple thanks to the  abacuses (simplified precomputations to be used graphically).


Turn-Based versus Real Time
---------------------------

Real tactical combat is usually turn-based, each player having as much time as needed to choose its options. Other actions (ex: exploration) are most of the time in real-time.

Life itself is in real-time, and the turn-based approach raises issues in a multiplayer context: while some characters would experience slow-passing time during turn-based combats, others could be able to explore the world as full speed. What if they were to interact with characters fighting in their time bubble ?

At the very least, each turn should be bounded in strict time limits, so that combat time do not drift too much from the game time.

For example, all the players involved in a combat would decide privately and concurrently of their actions, which would be triggered automatically by Orge only at the end of the turn. If a player takes too much time to decide his actions, the game automatically skips to the next turn.
 
Orge uses this kind of "relaxed" real-time system. Beware to bathroom breaks!


Place of Random
---------------

We chose to introduce some randomness when resolving actions, as it creates a kind of "dramatic tension". It forces oneself to evaluate every possible outcome and to ponder them painfully.

Regarding game content, having it generated based on randomness might be a bit hazardous: roaming through endless self-similar dungeons is not deemed fun enough. Better have a few well-crafted dungeons, authored by real-life designers, rather than infinite dull ones.

Same reasoning applies to random-generated quests, too many "fetch the orb from evil Wizard X" quests would kill most of the fun. 

One relevant area for randomness in game content is the terrain generation: well chosen fractal algorithms lead to realistic landscapes, on which human-generated content can be settled down for the better of both worlds.

Another interesting use of randomness is for the spawning of creatures: even if respawn areas should better be built by hand, they are then populated automatically. Otherwise monster populations would not be replenished, or too much manual tweaking would be necessary. See also: `Respawn`_.

A right balance between random encounters and pre-scripted confrontations must be found, both are needed.



Realistic versus Epic Setting
-----------------------------

Depending on the interactive story to be told, some actions might be totally impossible or, only, very unlikely (this is a difference of nature). The more unlikely outcomes may happen, the most epic the story will be.

With Orge we prefer to keep a certain level of "epicness", should such word exist. Thus extraordinary actions are always possible (notably thanks to the modifier system we use), even if they are very unlikely.

Too much realism would as well hinder the entertainment, thus relatively uninteresting facts of life (ex: the need to go regularly to toilets) have been abstracted out. See also: `Character Needs For Survival and Well-Being`_.


Tragedy versus Comedy
---------------------

We preferred letting the game system be, as much as possible, tone-agnostic: the choice of the mode of fiction is left to the scenario writer. See also: `Place of Death`_.
 

Medieval, Contemporary, Futuristic, etc.
----------------------------------------
The majority of popular MMORPGs are based on traditional fantasy themes, often occurring in an in-game universe comparable to that of Dungeons & Dragons.

Some employ hybrid themes that either merge or substitute fantasy elements with those of science fiction, sword and sorcery, or crime fiction.

Orge aims for the moment more specifically to fantasy settings. Later science-fiction themes could be explored. Contemporary stories are maybe a little less attractive.


Party & Singleton
-----------------

In some games the player identifies to a single hero (a singleton), whereas on others he controls a team of adventurers, i.e. multiple characters (a party). It is a completely separate concern from the single player/multiplayer design issue.

For the moment Orge focuses on singleton-based games, although team management can be interesting too, when building with long term planification each character so that it complements the others.


Role-play
---------

It is a corner stone of a successful online RPG. 

To help the development of roleplay, players are encouraged to customize their characters, notably thanks to a textual description.

Role-play should not be confused with social interactions, since there is a difference between immersive games and instant messengers. We want here to promote contextual exchanges, based mostly on in-game events.

Some game facilities are provided for that:

  - real-time IRC-like internal chat system, for interlocutors able to speak directly (in hearing range)
  
  - bulletin board systems, post, for asynchronous unreliable long-distance communication (couriers are expensive, and can be intercepted or dishonest)

  - a guild system, to structure the social exchanges
 
  - a stage direction system, allowing the player to specify attitudes and actions that cannot be expressed by the game system, a bit like e-motes. For instance: *Gurg leans on the bar, exhausted*.
  
An additional way of promoting roleplay is to delegate it, at least partially, to a real-life game master, whose role is to better adapt the game's reactions to the acting effort of players, for instance by awarding experience bonuses for good roleplay or by improving the NPC behaviours in the face of dialogs. 

This requires the Orge system to support, beyond the player role, the game master role, and to provide specific tools for it, like the ability for a game master to take control of one or more creatures (NPC, monsters, etc.). 

Other measures can be taken to further enhance the roleplay, like making mandatory the formation of groups of players, in the face of adversity, either because the opponents are too strong or because the challenges require specialized complementary skills (ex: detector, tank, damage dealer, healer, buffer, etc.). Group spells (ex: team buffs) help there too.
  

Ending
------

Single player and multiplayer non-persistent games should have at least one successful ending, and more probably numerous ones, with various levels of success and failure, some depending on goals preferred by the player during game.

Multiplayer games in a persistent world should have a far increased lifespan, less related to specific stories reaching completion. However all simulated worlds, including MMORPG, will have an actual termination in real life, and it should be preferably brought by a scenarized in-game final fireworks for a memorable ending.

Unethical endings may or may not be discouraged by the game system.


Time
----

Time is internally managed in rounds, i.e. unsigned integer simulation ticks. There is a strict relation between user time (wall-clock) and virtual (game) time: this simulated time flows 8 times as fast as the user one. Thus a virtual day lasts for 3 hours.


Passage of time
---------------

This is another major game element, directly linked to the `Place of Death`_, to `Aging`_ and, to a lesser extent, to `Object Wear`_. The time cannot be stopped, and flows quite fast.

The game world is persistant, but most beings are relatively short-lived, and a trade-off must be found between youth and experience.



Aging
-----

Another significant factor weighing on a character's life is the age of that character. Age is managed thanks as an unsigned integer number of years, and updated once per simulated year.


Life Expectancy
...............

Should no brutal death occur, any creature will die when its life expectancy has been reached. The creature lifespan is determined at its creation (see the *Longevity* primary attribute), and is generally only known by the game system. 

Life expectancy is computed the following way. To the average lifespan corresponding to the species of the creature, modifiers are added:

 - the constitution modifier of that creature
 - a 8% bonus if it is a female creature, otherwise a 8% malus
 - a random modifier in the -10% to 10% range
 
As stated in the `orders of magnitude for time`_ section, life expectancy is medieval times was quite shorter than nowadays, approximately 40 years. Thus if one played with one's character for three years, the character actually gained 24 years, i.e. spent more than half of its life in the meantime. 



Impact of Age on Abilities
..........................

During the life expectancy of a character, various stages will be reached, affecting his abilities (actually, his primary atributes), physical as well as mental, through an age multiplicative modifier. 

For example, a creature whose base strength is 40 and whose age results in a modifier of +20% will be having currently an actual strength of ``40 + 0.2*40 = 48``.

Globally, for a human whose lifespan is 40 years (average one for a human in Orge), the age modifier will start from -90% in the first year of the character, increase steadily until a threshold of maturity is reached (at around 16 years). This will correspond to the peak abilities (modifier: +120%). They will then decrease a bit until stabilizing in a plateau (about 22 years) with a null modifier (+0%). They will remain in this good condition until about 30 years, where they will begin to decline back to zero, not unlike in a `Attack Decay Sustain Release <http://en.wikipedia.org/wiki/ADSR_envelope>`_ scheme:


This global aging profile in Orge is common to all species, genders, etc.: the same evolution will apply for all creatures, once scaled according to the effective planned lifespan of each creature. For example, if Dwarves on average live for 320 years and if a given Dwarf, Hgog, is expected to live for 355 years [#]_, then his abilities will reach zero only when being 355-year-old (not 320).

.. [#] Determined by various factors, including gender, constitution and some randomness. 
 

The impact of aging is taken into account in the form of a modifier, see `Resolving Actions`_.

Conversely, being older might imply being more experienced, trading weaken statistics against improved skills.



Textual Translation of Age
__________________________


The following *Age Table* allows to determine the age-related textual description of a creature, based on its *Age Percentage*, which is equal to its current age divided by the life expectancy of its species [#]_ :


+------------------------+---------------------------+-------------------+---------------------+
| Age Percentage Range   | Creature Age Classifier   | Corresponding     | Alternate Namings   |
|                        |                           | Human Age         |                     |
|                        |                           | For Modern Times  |                     |
+========================+===========================+===================+=====================+
| 0% - 2%                | Newborn                   | 0 - 1 years       | Toddler             |
+------------------------+---------------------------+-------------------+---------------------+
| 2% - 5%                | Child                     | 1 - 4 years       |                     |
+------------------------+---------------------------+-------------------+---------------------+
| 5% - 10%               | Boy/Girl                  | 4 - 8 years       |                     |
+------------------------+---------------------------+-------------------+---------------------+
| 10% - 17%              | Youngster                 | 8 - 14 years      |                     |
+------------------------+---------------------------+-------------------+---------------------+
| 17% - 25%              | Adolescent                | 14 - 18 years     |                     |
+------------------------+---------------------------+-------------------+---------------------+
| 25% - 40%              | Young Adult               | 18 - 30 years     | Young               |
+------------------------+---------------------------+-------------------+---------------------+
| 40% - 55%              | Adult                     | 30 - 50 years     | (no special naming) |     
+------------------------+---------------------------+-------------------+---------------------+
| 55% - 80%              | Aged Adult                | 50 - 65 years     |                     |
+------------------------+---------------------------+-------------------+---------------------+
| 80% - 100%             | Elder                     | 65 - 80 years     |                     |
+------------------------+---------------------------+-------------------+---------------------+
| 100% - 120%            | Venerable                 | 80 - 96 years     |                     |
+------------------------+---------------------------+-------------------+---------------------+
| 120% - 150% and upward | Ancient                   | 96 - 120 years    |                     |
| [#]_                   |                           |                   |                     |
+------------------------+---------------------------+-------------------+---------------------+

  
.. [#] The creature's own life expectancy is not taken into account here: not all creatures could reach the *Ancient* age classifier.

.. [#] Unless specific conditions are met during the lifespan of a creature (ex: special magic used), its life expectancy should not exceed 150% of the average one defined for its species.


For example, knowing that dwarves live on average for 320 years, and that Hgog is a 272-year-old dwarf, he could be named *Hgog, Elder Dwarf*, as his age percentage is ``272/320 = 85%``. 


Rookie/Freshman (Novice)/Apprentice (Untrained)/Trained/Seasoned/Veteran/Master/Elite


Junior/Senior/Doyen

Cadet/


Example: 'Arthur, Young Human, Untrained Soldier' might become 'Arthur, '


Place of Death
---------------

Most RPG insist on character building and provide some ways of bypassing the total loss of one's character, as it would be deemed to frustrating for a player to have to start from scratch again.

On non-persistent RPG, saving the game regularly (with ou without restriction) is a technical solution. On persistent RPG, time cannot be changed at will and often various solutions are implemented directly in the gameplay, like resurrection in temples, afterlife in the wild, etc.

One considerable drawback of non-terminal death is that it allows players to act without much fear for the consequences: for example there is no point in an assassination attempt on a king if, even in case of success, he will able to be back within minutes, as if nothing had happened.

On the contrary, if permanent death (a.k.a. `permadeath <http://en.wikipedia.org/wiki/Permadeath>`_) was retained, then that king would most probably be a lot more careful when travelling, would have to select carefully his escort (powerful and loyal guards), there could be plots among his vassals, etc. This kind of power struggle would benefit a lot to gameplay, and would require the player a lot more of self-control and constant care for his character, lest it is removed permanently from the game .

Thus in Orge we favour permadeath. Note that it tends to make a character's health binary, being either in perfect condition or dead, as the role of permanent injuries is considerably lessen thanks to magic. A related concern for players is that "perfect condition" is to be understood regarding a given age.




Ranks
-----

Promotion is usually obtained thanks to achievements (i.e. is often based on experience points).
 
 
Military Ranks
..............

This hierarchy is based on following ranks:

 #. Private
 #. Second Class
 #. Corporal
 #. Sergeant
 #. Sergeant Major
 #. Lieutenant
 #. Captain
 #. Major
 #. Lieutenant Colonel
 #. Colonel
 #. Brigadier
 #. General
 #. Field Marshal
 #. Amiral

The military rank directly impacts the montly wage, among other things.


Religious Hierarchy
...................

The secular authority, which involves legal and military authority, should not be mixed up with the clerical authority.

Secular Hierarchy
_________________

 - Bishop
 - Cardinal
 - Pope
 
Clerical Hierarchy
__________________

 - Novice
 - Monk

 
Levels of Expertise In Martial Arts
...................................

 - Untrained
 - SwordMaster
 - Grand Master


Age
...

See `Textual Translation of Age`_.




Client/Server, Model/View/Control and al
----------------------------------------


Recurrent Declaration about the Importance of Roleplay
--------------------------------------------------

Each and every game system, after dozens of pages involving equations and algorithms, ends up with the finding that all that matters is storytelling (on the GM side) and roleplaying (on the player side). There will be no exception here.

Citation




Creature Characterization
=========================

Not depending on a creature being a humanoid, a sentient being, a NPC, a monster or a PC, it will be described in an uniform way, by the means of *character's statistics* (a.k.a. "stats").


Gender
------

Most creatures are either male or female, permanently. This has an impact on attributes.


Traits
------

Alignment
---------

Alignment is not chosen as such by the controller of a creature: it just results from its beliefs, choices and actions.

Alignment is thus considered in Orge as a minor indicator. 

For the vast majority of creatures, it is overshadowed by reputation, as these creatures cannot see directly in the system of values of a target character, and therefore can only rely on observation and reputation.

Alignment is useful though for telepaths and, more importantly, deities, which can check to which extent their followers respect their expected approach to life.


In Orge, alignment is determined quite classically from two measures:

 - a **moral** balance, which ranges continuously from good to evil:
 
	1. *good*: compassion and search for the common good
	2. *neutral*: selfishness and lack of interest for others
	3. *evil*: taste for the suffering of others
 
 - a **psychosocial** balance, which ranges continuously from lawful to chaotic:
 
	1. *lawful*: loyal, reliable, rational, stable, logical, having respect for all kinds of rules, but also dull, predictable, narrow-minded, unimaginative 
	2. *neutral*: balanced, but also indifferent or lacking the capacity to judge
	3. *chaotic*: original, free, surprising, creative, but also offensive, changing, untrustworthy, disturbing, contradictory, disorderly, arbitrary
	
See also the  nine combinations, as detailed this Wikipedia `article <http://en.wikipedia.org/wiki/Alignment_(Dungeons_%26_Dragons)#Alignments>`_, in the rather close context of *Dungeons & Dragons*.
	
	

+-------------+-------------------------------------------+-------------------+
| Creature    | Description                               |         |
| Alignment   |                                           |                   |
| Alignment   |                                           |                   |
+=============+===========================================+===================+
| Lawful      |       |          |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Neutral     |       |          |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Chaotic     |       |          |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+

Good and evil characters cannot join the same party.


Species
-------

In Orge *Species* is preferred to *Speciess*. No distinction is made between sentient, intelligent, advanced creatures, animals, monsters, etc. They are all creatures of a given species.


Species Characteristics
....................


+------------------+----------------+--------------------------------------------------+
| Species             | Species 		    |Description  							           |
| Characteristic   | Characteristic |                                                  |
| Name             | Usual Acronym  |                                                  |
+==================+================+==================================================+
| SpeciesWeightFactor | RWF 		    | A creature too much loaded will grow tired       |
|                  |                | sooner, depending on its species.                   |
|                  |                | See `Fatigue Due To Carried Weight`_.            |
+------------------+----------------+--------------------------------------------------+



+-------------+-------------------------------------------+-----+--------------+
| Creature    | Description                               | RWF |              |
| Speciess       |                                           |         |              |
+=============+===========================================+=====+==============+
| Human       |       |          |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Elf         |       |          |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Dwarf       |       |          |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Gnome       |       |          |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Halfling    |       |          |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Goblin      |       |          |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Orc         |       |          |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+


.. 

Table of species statistics

Life base expectancy, in years


Professions
-----------

There is no such thing in Orge like Character Classes: Character Professions are introduced instead.

Professions allow only direct characterization of a creature thanks to the use of archetypal, stereotyped profiles based on *skills*.

Professions do not belong to the in-game notions managed by the Orge system, they are just a convenience to build characters or to identify them: depending on a character know-how (skills), the game system can determine whether a character is a druid and/or a thief, etc.

Professions are stored hierarchically, in the *Career Tree*.


+-------------+-------------------------------------------+-------------------+
| Name of the | Description                               | Subclasses        |
| Base        |                                           |                   |
| Profession  |                                           |                   |
+=============+===========================================+===================+
| Fighter     |       | Barbarian, Warrior        | Melee Fighter, Duelist, Berseker, Archer
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Cleric      |       |          |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Wizard      |       | Mage, Sorcerer, Necromancer         |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Thief       |       |          |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Rogue       |       |          |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Bard        |       |          |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Druid       |        |          |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+


Sub-classes
-----------

+-------------+-------------------------------------------+-------------------+
| Name of the | Description                               |     |
| Base Class  |                                           |    |
+=============+===========================================+===================+
| Barbarian   | Berseker      |          |
|             |                          |                   |


Multi-classing
--------------

Some hybrids of the base classes are built-in: 

+-------------+-------------------------------------------+-------------------+
| Name of the | Description                               | Hybrid Of         |
| Hybrid Class|                                           |                   |
+=============+===========================================+===================+
| Bishop      |       | Priest/Wizard         |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Samurai     |       | Fighter/Wizard         |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| WarLord     |       | Fighter/Priest        |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+
| Ninja       | Fighter that shuns weapons and excels at critical strikes, ex:  Monk   | Fighter/Thief         |
|             |                          |                   |
+-------------+-------------------------------------------+-------------------+


Classes & Alignment
-------------------

Regarding classes, some alignment restrictions apply: 

	- a Rogue cannot be good
	- a Bishop cannot be neutral
	- a Samurai cannot be evil
	- a Warlord must be good
	- a Ninja must be evil







Characteristics
---------------

The characteristics of a creature are determined by a set of attributes. Most attributes are positive unit-less integers, with no upper limit, although attribute values beyond 100 should be very, very rare.

The characteristics of a creature are split in following attribute sets:

 #. primary attributes
 #. secondary attributes
 #. state attributes
 #. abilities
 #. skills

 
All characteristics are declared in class_Creature.hrl_.
 
 

Primary Attributes
..................

The core potential of a creature is described by following *primary attributes*:
 
 
+-------------+-------------------------------------------+-------------------+
| Name of the | Meaning and Use [#]_                      | Usual Synonyms    |
| Primary     |                                           | and Close Terms   |
| Attribute   |                                           |                   |
+=============+===========================================+===================+
| Strength    | Physical force, vigor, power              |                   |
+-------------+-------------------------------------------+-------------------+
| Agility     | Power of moving the limbs quickly and     | Dexterity,        |
|             | easily; nimbleness                        | Accuracy          |
+-------------+-------------------------------------------+-------------------+
| Constitution| Ability to withstand fatigue, disease,    | Endurance,        |
|             | deprivation, etc., and continue working   | Hardiness,        |
|             |                                           | Stamina           | 
+-------------+-------------------------------------------+-------------------+
| Intelligence| Readiness of comprehension                | I.Q.              | 
+-------------+-------------------------------------------+-------------------+
| Wisdom      | Knowledge, and the capacity to make due   | Discernment,      |
|             | of it                                     | judgment          |
+-------------+-------------------------------------------+-------------------+
| Willpower   | Power of the mind by which we decide to do| Will,             |
|             | or not to do                              | Self-control,     |
|             |                                           | Courage           |
+-------------+-------------------------------------------+-------------------+
| Charisma    | Personal attractiveness that enables      | Appeal,           | 
|             | to influence others                       | Attractiveness    |
|             |                                           | Authority, Charm, |
|             |                                           | Leadership,       |
|             |                                           | Suggestion        |
+-------------+-------------------------------------------+-------------------+
| Quickness   | Rapidity of action (speed) and wit        | Speed, Sagacity,  |
|             | (mental alertness)                        | Initiative,       |
|             |                                           | Sharpness         |
+-------------+-------------------------------------------+-------------------+
| Longevity   | Expected length of life                   | Lifespan          |
+-------------+-------------------------------------------+-------------------+

.. [#] Most definitions are taken from various dictionaries, like the Webster.




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


This applies independently from the species.


+-------------+---------------+-----------------+
| Name of the | Male Modifier | Female Modifier |
| Primary     |               |                 |
| Attribute   | 			  | 			    |
+=============+===============+=================+
| Strength    | +8			  | 0			    |
+-------------+---------------+-----------------+
| Agility     | -2			  | +2			    |
+-------------+---------------+-----------------+
| Constitution| +5			  | 0			    |
+-------------+---------------+-----------------+
| Intelligence| 0			  | 0			    |
+-------------+---------------+-----------------+
| Wisdom      | -4			  | +4			    |
+-------------+---------------+-----------------+
| Willpower   | +3			  | 0			    |
+-------------+---------------+-----------------+
| Charisma    | 0			  | +3			    |
+-------------+---------------+-----------------+
| Quickness   | +2			  | 0			    |
+-------------+---------------+-----------------+
| Longevity   | 0			  | +10			    |
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
| Fatigue     | Physical and mental ability to overcome   | See below         |
| Model       | fatigue, by resistance and recovery       |                   |
+-------------+-------------------------------------------+-------------------+
| Movement    | The maximum walking speed under nominal   | Strength,         |
| Rate        | conditions                                | Agility           |
+-------------+-------------------------------------------+-------------------+
| Nominal     | The maximum carried weight with no        | Strength,         |
| Carried     | movement penalty. A creature will not be  | Constitution      |
| Weight (NCW)| able to move at all if loaded with        |                   |
|             | ``SpeciesWeightFactor*NCW`` kilograms or  |                   |
|             | higher. See                               |                   |
|             |`Fatigue Due To Carried Weight`_.          |                   |
+-------------+-------------------------------------------+-------------------+


Fatigue Model
_____________


The fatigue model is parametrized by two pairs of values regarding physical and mental:
	 
 - maximum sustainable fatigue (`max_physical_fatigue` and `max_mental_fatigue`)	  
 - recovery rate (`max_mental_fatigue` and `mental_recover_rate`)


The fatigue attributes of a character are determined at character creation, from a base value computed from the primary attributes and affected by various modifiers.


+-------------------------+----------------------------------------------+-------------------+
| Name of the             | Base Value                               	 | Potential		 |
| Fatigue                 |                                          	 | Modifiers		 |
| Attribute               |                                          	 |  				 |
|                         |                                          	 |  				 |
+=========================+==============================================+===================+
| Maximum Physical        | ``Constitution * 5 + Strength * 2 + Agility``| Species, Traits,  |
| Sustainable Fatigue     |                                          	 | Buffs			 |
+-------------------------+----------------------------------------------+-------------------+
| Maximum Mental          | ``Willpower * 5 + Intelligence * 2 + Wisdom``| Species, Traits,  |
| Sustainable Fatigue     |                                          	 | Buffs			 |
+-------------------------+----------------------------------------------+-------------------+
| Physical Recovery       | 5 physical fatigue points per round      	 | Species, Traits,  |
| Rate                    |                                          	 | Buffs			 |
+-------------------------+----------------------------------------------+-------------------+
| Mental Recovery         | 5 mental fatigue points per round        	 | Species, Traits,  |
| Rate                    |                                          	 | Buffs			 |
+-------------------------+----------------------------------------------+-------------------+

  

This model is declared in class_Creature_, with the `creature_fatigue_model` record.

See the `Fatigue`_ section for further details.



Movement Rate
_____________


The base walking speed of a character is equal to ``Agility * 2 + Strength``.
It is further modified by species, 


Nominal Carried Weight
______________________



Creature State
--------------

Contrary to characteristics (primary/ssecondary attributes), which are rather static, there are other attributes that reflect the changing state of a creature.

These *state attributes* are:

  - current age
  - health
  - location in virtual world




Abilities & Traits
------------------

Some characters are able to perform specific actions due to their nature. This includes for instance telepathy, or the ability of destroying armor by mere contact. This corresponds to *abilities*. 


+----------------------------+-----------------------------------------------+------------------------------------+-------+
| Name of the                | Description                                   | Effect       			 	      | Cost  |
| Ability / Trait            |                                               | 		                     	      |       |
+============================+===============================================+====================================+=======+
| Armor Decomposition        | When the creature touches a piece of          | Each direct touch from the	      | 1 CCP |
|                            | armor, the armor is permanently weaken [#]_   | creature deals 20 CWL to  	      | 	  |
|                            |                                               | the target armor          	      | 	  |
+----------------------------+-----------------------------------------------+------------------------------------+-------+
| Empathy                    | The creature may be able to feel some         | Strength of emotion    	 	      | 5 CCP |
|                            | emotions of close creatures                   | perception increased by 	 	      | 	  |
|                            |                                               | 80% 						 	      | 	  |
+----------------------------+-----------------------------------------------+------------------------------------+-------+
| Telepathy                  | The creature may be able to project           | Strength of emotion projectiond    | 5 CCP |
|                            | emotions of close creatures                   | increased by 80%				      | 	  |
+----------------------------+-----------------------------------------------+------------------------------------+-------+
| Enduring                   | The creature is an exceptionally enduring     | + 6 physical FP recovery per round | 4 CCP |
|                            | specimen of its species, physically           | + 2 mental FP recovery per round   | 	  |
+----------------------------+-----------------------------------------------+------------------------------------+-------+
| Tough                      | The creature is an exceptionally tough        | + 2 physical FP recovery per round | 4 CCP |
|                            | specimen of its species, mentally             | + 6 mental FP recovery per round   | 	  |
+----------------------------+-----------------------------------------------+------------------------------------+-------+
| Light Foot                 | The creature can move exceptionally           | + 25% in all movement rates        | 4 CCP |
|                            | efficiently for its species                   | + 10% in stealth movement    	  | 	  |
+----------------------------+-----------------------------------------------+------------------------------------+-------+

.. [#] See for example "gelatinous cubes" in some games.

 
 
Skills
------ 


Skill Definition
................

A skill defines the capacity of a character to perform a specific kind of action, regarding the technic, the know-how. 

For example, for a successful horse-riding, following needs can be identified:
  
  - the player must be strong and agile enough
  - he should be concentrated enough on his task
  - he should have enough practise
  
The first two points are dictated by the character attributes only (ex: strength, agility, willpower), whereas the last one corresponds to a know-how, the horse-riding skill. 

Only the know-how that cannot be expressed by roleplay should be captured by skills. That is the case of pratical knowledge like horse-riding, or non-translatable one like mastery of oral Orc language. Conversely, philosophy or debating should not be skills, as they depend more on the player than on his character: if a player were a good debater and its character a poor one according to its attributes, the overall result would be still that the character . 

A corollary is that even if the game system ensures that all characters are initally equal statistics-wise, due to the players this will not be true in-game.


Skill Progression
.................

An action generally leads to a slight increase in the associated skill(s), whether the action succeeds or not. However the gain is four times smaller if the action failed.

This models the fact that, for most actions, the more one practises, the better at it one becomes, knowing that even failures teach lessons.

Thus skills, unlike experience which is threshold-based, follows a continuous, almost invisible, progression.


Skill Tree
..........

Skills are sorted hierarchically, in the *Skill Tree*. The reasons for that sorting is that most skills can be refined again and again in more specialized skills.

For instance, if a player has a strong horse-riding skill, then he should be quite good at warhorse-riding, but nevertheless weaker than the same character who would have invested all his efforts directly in warhorse-riding. On the other hand, the first one should be able to ride more effectively all kinds of horses than the second one.

Orge skill tree is the following:

 - horse-riding
 - taming
 - martial arts

 
Interactions
------------


Intimidation
............


When a creature notices other creatures, it may be scared by this encounter. The reaction of the creature depends on:
 
 - its experience level
 - its mental fatigue
 - on the intimidation factor of the met creatures, which is the sum of the intimidation factors of all the creatures it the group

Should the creature fail the intidimidation test, it can suffer from an action penalty the next rounds, or be completely paralyzed by fear.

 


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


Character Needs For Survival and Well-Being
-------------------------------------------

A character would suffer from the lack of:

 - food
 - water
 - oxygen
 - sleep
 - light

If fun and demanding at first, micro-managing all these resources might become tedious. A way of introducing them initially while in the long term freeing the successful player from their management is to offer more expensive but less cumbersome replacements for them.

This can be done for example by ensuring that the loot of most high-ranked monsters includes rich food (ex: dragon steacks), that advanced dungeons levels have a minimal density of drinkable fountains, and that, instead of torches to be bought, transported and replaced, either sustainable oil lamps or lanterns or relatively cheap spells of light are available.


Health, Wounds and Death
------------------------


Fatigue
-------

Orge offers a real-time fatigue system. Physical and mental (psychological) exhaustion are internally managed, they are quantified with ``Physical Fatigue Point`` (PFP) and ``Mental Fatigue Point`` (MFP). Note that the latter has nothing to do with mana points, it applies as well to non-magical users.

The current overall state of fatigue and stress of the main character is reported to the player only by the audio feedback of a heart beating quicker and quicker - until fainting (and likely becoming monster meat). Thus the player has to discriminate himself between the two kinds of fatigue.

Taking damage, or moving too quickly and too much loaded will cause the heart to pulse rapidly, as ambushes and horrific sights will do.

The different causes of fatigue adds up, and may lead to the character exhaustion or unability to act without resting.

The fatigue system is internally managed that way, for each kind of fatigue:
 
 - each creature has a ``Base Fatigue Budget`` (BFB) and a ``Fatigue Recovery Rate`` (FRR), which are determined by its specy, its statistics (notably its Constitution), its age
 
 - each creature is born without fatigue, but its various actions have a fatigue penalty, depending on many factors including a base cost (depending on the action) and context-depent additional costs which may depend on weight carried, temperature, age, state of health (if the character is wounded, or poisoned, etc.)
 
 - each round, each creature having non-null fatigue points (FP) have them decreased by FRR
 
 - a creature is unable to perform any action which would result in having a fatigue budget higher than its BFB
 
 
 
One can picture the fatigue system as a water bucket whose maximum level is the BFB, with a leak equal to the FRR, which starts initially empty but is filled a bit as each action of the character. The goal is to avoid that the water level reaches the top of the bucket.


Fatigue is managed in class_Creature.erl_ and tested in class_Creature_test.erl_.

 
 
Fatigue Due To Carried Weight
.............................

Knowing that ``RWF`` is the ``Species Weight Factor`` and ``NCW`` is the character ``Nominal Carried Weight``:

 - a static (non-moving character) can carry up to ``RWF * NCW`` kilograms
 - a walking character can carry up to ``NCW`` kilograms without additional fatigue penalty
 - a running character can carry up to ``NCW/4`` kilograms without additional fatigue penalty

Between these thresholds, fa

Let's take the example of Ulf the Gnome, whose ``NCW`` is equal to 4 (it is a strong gnome indeed). As a gnome, Ulf has a ``RWF`` of 2.

Thus: 

 - Ulf can carry without moving no more than ``RWF * NCW = 8 kilograms``
 - he can walk with up to `NCW = 4 kilograms`` without additional fatigue

évaluer les points de fatigue 
 


Loot & Possessions
------------------
 
Another kind of reward can be the loot given by a defeated creature, either directly (ex: after being itself searched or after a tribute is given for surrender), or indirectly (ex: treasure in a chest, in the creature dwelling). 

By default in Orge (i.e. unless the scenario specifically changes that behaviour), a searched creature will show exactly its expected belongings [#]_, instead of having the SW or GM specify explicitly what the creature is to drop.


.. [#] It sounds obvious, but in many games when one finally succeeds in killing a powerful knight, no weapon can be retrieved, which is totally unrealistic. Conversely, it would be quite awkward to find gold coins on wild animals.


Respawn
-------

Most of the creatures in Orge are expected to be specifically instanciated by the SW. When they die there is no reason they should be replaced.

However in some cases it might be convenient to define `respawn points` or `respawn areas`, i.e. locations where a certain density of certain creatures has to exist. This is especially true for MMORPG, where players might intentionally exhaust an area whereas this is only an artefact of the simulation. For example, in the edge of a huge and deep forest, a respawn area could be defined, instead of having to simulate each and every squirrel in that forest. 

These respawn locations should be used with care, since they are often abused by players to level up and/or to block other players from doing so. It is up to the SW to tell whether, for his story, this should be possible.


Perception & Senses
-------------------


Character Creation
------------------

If the player is given the ability of customize the characteristics of a creature she controls, she will have in most cases a budget (expressed in Character Points) to spend on primary attributes (including age) and skills.


Age vs Skills Trade-Off
.......................

The player might prefer to create a middle-aged character (thus having decreased actual statistics, including lifespan) who would compensate with augmented skills. Sometimes relying on a trained thief is safer than to bet on a promising yet unexperienced burglar.

Some professions (ex: soldier) require usually that characters are trained for some years (ex: for 16 to 20-year-old) before being acknowledged competent for a certain rank (see `Military Ranks`_).



Resolving Actions
=================

What for?
---------

In a game, the success of all kinds of attempts of actions have to be evaluated, like when: 

 - attacking a foe
 - parrying an attack 
 - climbing a wall, etc.


The game system has to choose whether each action succeeds or fails.


How?
----

We based our system on the one Hirinkaël described `here <../../../club/game/numericBook/ThresholdSimulationSystem.pdf>`_ (in French), most choices and remarks regarding the evaluation of actions are just taken from this document, adapted a bit and translated. 


It is done by evaluating the probability p of success (say, between 0 and 100%), and drawing a random value r in, ``[0,100]``. If ``r<p``, then the action succeeds, if ``r>p`` it fails, otherwise ``r=p`` and the outcome is neither a success nor a failure (for the sake of simplicity we will consider from now on that the action is then a success).

The core of the problem is to have the game system select an accurate, realistic value for p, the probability of success for that specific action.

The intended outcome of this part of the rule system is not a predetermined "one size fits all" probability of success: we want a probability that matches closely the set of specific and hardly predictable circumstances that exist at this moment of the game.

As the game creator cannot precompute all the possible contexts (as they are too numerous, due to the combinatorial explosion of possible in-world variations), the game system has to rely on rules that can determine on the fly, in the light of a specific context, a global probability of success.
 

We chose to rely on `Hirinkaël's system <../../../club/game/numericBook/ThresholdSimulationSystem.pdf>`_ because:

 - it can easily take into account all kinds of situations
 - it is logical, consistent, symmetrical and fair
 - it is original
 - it is quite simple to understand and use
 - it can be used both in video games (thus being computer-based) and in table-top games (with dice and an abacus)
 
 
The reason we like this system lies mainly in the first advantage listed (flexibility, adaptability), as it offers us a way of adapting to an actual situation (real-life, complex, full of context) the chances of success of a given action which was modelled under normal circumstances (nominal conditions) only.


Two inputs: base probability & modifiers
---------------------------------------- 

Let's take, from the document we just mentioned, the example of a character wanting to attack another, and let's suppose we can evaluate the base probability, i.e. the probability of success of this action *under normal circumstances*.

The problem is that, in real cases, we are never in that stereotypical situation: the characters might be fighting in the dark, or one may be wounded, or there may be multiple opponents, etc. How to obtain, from the probability evaluated under normal circumstances, a custom probability reflecting all these numerous elements of context ?

What we really need actually is to be able to take into account probability modifiers that:

 - can be defined once for all, not depending on the base probability nor on any other modifier
 - can convert the base probability into the target context-dependent probability realistically (ex: it must be more difficult indeed to hit when wounded) and correctly (ex: probabilities must remain in the [0,1] range) 


One output: the actual context-dependent probability
---------------------------------------------------- 

To handle modifiers we retained the function suggested in the aforementionned paper:

nnn nn:raw-html:`<img src="probability-modifier-formula-negated.png"></img>`
nn nn:raw-latex:`\includegraphics[scale=0.75]{probability-modifier-formula.png}`


This function, when given a base probability (p, abscissa) and a modifier (m, select the corresponding curve), determines the resulting modified probability (pm, ordinate):

nn:raw-html:`<img src="probability-modifier-negated.png"></img>`
nn:raw-latex:`\includegraphics[scale=0.75]{probability-modifier.png}`



The wider curves, representing modifiers of -50%, 0% and 50%, allow to find easily the curves for intermediate modifiers, as they are paced every 10%.

One can see the symmetry of modifiers: the value of the increase in probability due to a given modifier p is equal to the value of the decrease in probability due to a modifier of -p.

The `probability-modifier.py script <http://osdl.svn.sourceforge.net/viewvc/osdl/OSDL/trunk/src/doc/web/main/documentation/OSDL/OSDL-engine/probability-modifier.py?view=markup>`_ gives some more indications:

::

  For a base probability of success of 0.0 %, with a modifier of:
    -30.0 %, modified probability is 0.0 %.
    -20.0 %, modified probability is 0.0 %.
    -10.0 %, modified probability is 0.0 %.
    0.0 %, modified probability is 0.0 %.
    10.0 %, modified probability is 0.0 %.
    20.0 %, modified probability is 0.0 %.
    30.0 %, modified probability is 0.0 %.

  For a base probability of success of 25.0 %, with a modifier of:
    -30.0 %, modified probability is 8.63 %.
    -20.0 %, modified probability is 12.5 %.
    -10.0 %, modified probability is 17.9 %.
    0.0 %, modified probability is 25.0 %.
    10.0 %, modified probability is 33.6 %.
    20.0 %, modified probability is 43.5 %.
    30.0 %, modified probability is 54.0 %.

  For a base probability of success of 50.0 %, with a modifier of:
    -30.0 %, modified probability is 22.0 %.
    -20.0 %, modified probability is 30.1 %.
    -10.0 %, modified probability is 39.6 %.
    0.0 %, modified probability is 50.0 %.
    10.0 %, modified probability is 60.3 %.
    20.0 %, modified probability is 69.8 %.
    30.0 %, modified probability is 77.9 %.

  For a base probability of success of 75.0 %, with a modifier of:
    -30.0 %, modified probability is 45.9 %.
    -20.0 %, modified probability is 56.4 %.
    -10.0 %, modified probability is 66.3 %.
    0.0 %, modified probability is 75.0 %.
    10.0 %, modified probability is 82.0 %.
    20.0 %, modified probability is 87.4 %.
    30.0 %, modified probability is 91.3 %.

  For a base probability of success of 100. %, with a modifier of:
    -30.0 %, modified probability is 100. %.
    -20.0 %, modified probability is 100. %.
    -10.0 %, modified probability is 100. %.
    0.0 %, modified probability is 100. %.
    10.0 %, modified probability is 100. %.
    20.0 %, modified probability is 100. %.
    30.0 %, modified probability is 100. %.

  For a base probability of success of 60.0 %, with first modifier being 20.0 %,
   with second modifier being 15.0 %, modified probability is 86.7 %.
  For a base probability of success of 60.0 %, with first modifier being 15.0 %,
   with second modifier being 20.0 %, modified probability is 86.7 %.
  For a base probability of success of 60.0 %, with a modifier of 35.0 %,
  modified probability is 86.7 %.


We can see that if the base probability is 100% or 0%, the action will be always respectively a success or a failure, not depending on the modifier. 

Moreover, multiple modifiers can be applied, applying m1 then m2 results in the same as applying m2 then m1, which is (in practice) the same as applying directly ``m3 = m1 + m2``. 


How to choose the rule inputs ?
-------------------------------

If determining the base probability is part of the usual tasks of the seasoned Game Master, modifiers, as used here, are specific to this system.

The numerical rules have been calibrated so that, when base probability is at 50%, a modifier equal to m will result in a final probability equal to about ``50+m %``. 


So, to evaluate the value of a modifier modelling a specific element of context, the Game Master just has to imagine the case where:

 - we are under normal circumstances 
 - and the action is to be performed by a candidate who has 50% of chance of succeeding, 50% of chance of failing
 
 
Then, should the studied element of context occur, what would be the new probability of success (pnew) ? Once that value has been evaluated by the Game Master, the modifier is just equal to that value minus 50%: ``m = pnew - 50%``.

More precisely, the previous numerical recipe (``50+m %``) is an approximation, mostly accurate for small values of m. The real relation is: if the Game Master chooses, for the aforementioned probability of success, pnew, then ``m = 1 / (1+ exp(-pnew))``.

It leads to this abacus, helping to evaluate this modifier:

nn:raw-html:`<img src="modifier-abacus-negated.png"></img>`
nn:raw-latex:`\includegraphics[scale=0.75]{modifier-abacus.png}`


We can see that in most situations it is perfectly safe to stick to ``m = pnew - 50%``.


Let's illustrate with a simple example
--------------------------------------


Let's suppose that a given dark ninja has a nominal probability of climbing a given wall successfully equal to 80% (ninjas are somewhat gifted for that kind of exercise). Let's suppose as well that we are at night (note that it is a bonus for our dark ninja) but it is raining (malus) and sadly he has just been wounded by an alligator (big malus).  

As modifiers are made to be context-free, we need here to evaluate only the impact of a given modifier against the nominal situation, without having to bother with any other modifier (we suppose here all modifiers are independent, although one may argue it is an oversimplification here). 

Let's take the malus due to the wound. We may consider that a wounded dark-ninja at daylight, dry weather, incures a 35% penalty when trying to climb that kind of wall.

The same exercise may result in a 10% bonus for the climbing of an unwounded ninja, dry weather, but in the night, and in a 5% malus for the climbing of an unwounded ninja, during the day, but under the rain.

So, what is the overall probability of successful climbing, for our wounded ninja under the rain at night time ? Modifiers have to be summed, it results in a global modifier of ``-35+10-5=-30``. We then look at our curves: the intersection of the ninja base probability of success (80%) with the curve of the -30% modifier gives us a final probability of about 53%. 0ur ninja finally has little more chance to arrive at the top of the wall than to fall miserably in the flowers.



A Focus on Conflicts & Combats
==============================

One concern: to avoid that experienced characters based on fighting skills (barbarians, sword masters, ninjas, etc.) become significantly weaker, limited and dull to play than magic users, notably in late game.

Solutions: battle disciplines/maneuvers


Weapons 
-------

A weapon can be hand-to-hand and/or ranged.

All weapons - even hand-to-hand - have an attack range. For example an halberd allows for a larger range than a dagger.

Most weapons can be used different ways, to throw, stab, bash, slash, etc, called *attack style*.

This impacts notably on:

	- the rate of attacks
	- the probability to hit 
	- the probability to defend (block/dodge/parry)
	- the inflicted damages
	- the wear of the attacker weapon
	- the wear of the defender armor
	
All weapons, depending on the attack style, inflict a special kind of damage, in :


Damage Type

Punctual/Repeated (damage over time)

 - arcane
 - blade
 - cold
 - fire
 - impact
 
 - piercing
 - slashing
 - crushing (bludgeoning, blunt)
magical, elemental, poison,
Chopping and Bashing
Energy, heat, electrical/Lightning
acid

drain

Armor
-----

armor types


Defense

Resitance : modifiers depending on the damage type


Terrain modifiers


Magic
=====

Some spells (named also incantations) are directly combat-related, they are called *Battle Arcanes* (ex: lightning bolt).

Other spells are *Summoning Contacts*, whose role is to notify some very specific creatures to come to appear and, hopefully, to obey their summoner, as he is unable to control them directly.

Spells useful in various ways are called *Ascendant Incantations* (ex : teleportation).

Each spell, to be casted, requires a conscious caster, able to perform magic and having enough mana points, and having performed the right incantation (correct spell keys in the correct order). 

The player manual lists some basic keys and spells.
Lost mana points can be regained:

 - at a low pace, when enough time has elapsed, with significant bonuses obtained when resting or, still more, when meditating
 
 - in special places filled with magic (ex: some temples)
 
 - with specific elixirs or spells targeted by other magical users
 

The casting then may fail or succeed, on the emitter side, depending notably of the capacities of its caster.
  
Once successfully casted, a spell may fail or succeed, depending on the context (notably: target resistance). In all cases the mana is spent.


Spell Failure
-------------

Should a spell fail, on most cases nothing special will happen, spell will just fizzle.

On other cases, it may backfire, and deal damages, especially on the caster.

On rare cases, with some spells, the failure might result in various unwanted effects, immediately apparent or not.
  

Wizzard/Mage
------------

Cleric/Priest
-------------


Spell List
----------


+------------+------------------------------+--------------+----------+--------+
| Spell      | Spell Description            | Spell        | Header 3 |        |
| Name		 |                              | Target       |		  |        |
+============+==============================+==============+==========+========+
| Little     | As long as the spell lasts,  | None (caster)| Header 3 |        |
| Thumb [#]_ | the caster will drop small   | 		       |		  | 	   |
|            | arcane peebles (one every    | 		       |		  | 	   |
|            | ten seconds). Each one will  | 		       |		  | 	   |
|            | disappear around one hour    | 		       |		  | 	   |
|            | after it has been dropped.   | 		       |		  | 	   |
|            | It is a convenient way of    | 		       |		  | 	   |
|            | marking one's route, although| 		       |		  | 	   |
|            | some creatures tend to move, |		       |		  | 	   |
|            | collect or eat them [#]_.    | 		       |		  | 	   |
+------------+------------------------------+--------------+----------+--------+


.. [#] Inspired from the literary fairy tale of Charles Perrault, *Le Petit Poucet*.

.. [#] Any elemental eating arcane reification will be considerably strengthen.



Bestiary
========

The bestiary depends heavily on the scenario setting. It is mainly composed of `monsters <http://en.wikipedia.org/wiki/Monster>`_, in the traditional sense. A complete list can be found in `this <http://en.wikipedia.org/wiki/List_of_species_in_fantasy_fiction>`_ Wikipedia article.


Orge comes with the following built-in species [#]_, listed alphabetically in each theme:

.. [#] The SW can of course add any other creatures of his own. 


Main Sapient Beings
-------------------

 - Dwarf
 - Elf
 - Giant
 - Gnome
 - Goblin
 - Halfling
 - Human
 - Orc
 - Ogre
 - Troll
 

Animals
-------

 - Bat
 - Dragon
 - Griffin
 - Manticore
 - Unicorn
 - Monkey 
 - Spider
 
Undead
------

 - Ghoul
 - Lich
 - Vampire
 - Zombie
 - Skeleton
 - Specter
 - Wraith
 
 
Monsters
--------

 - Basilisk
 - Gargoyle
 - Imp
 - Wraith
 - Banshee
 - Beholder 
 - Chimera
 - Cyclope
 - Golem
 - Demon
 - Djinn
 - Doppelgänger 
 - Gorgon
 - Harpy
 - Kobold
 - Hydra
 - Kraken
 - Giant Spider
 - Mermaid
 - Minotaur
 - Phoenix
 - Sphinx 
 - Lizard Man
 



Creature Behaviours
-------------------

Seducing
........

In Orge a creature can attempt to seduce another creature of the same species but of the opposite gender, thanks to non-verbal communication. In case of success, the seduced creature cannot attack the seducing one.



Stealing
........

Some creatures may tend to steal the possessions of other creatures.
There are all-purpose kleptomaniacs (they will try to rob all kinds of objects) and specialized ones (ex: only foods interest them).


Frightening
...........

Some monsters are so disturbing lifeforms for a non-accustomed observer that she may be paralysed or even, in the case of abnormal abominations (Cthulu-like creatures), be scared to madness or to instant death. 


Pets
----

Some species tend to be tamable.



Character Roles
===============

 - Merchant, Vendor
 - Guard, Sentinel




Object Repository
=================

Each object is determined by:

 - a name
 - a textual description
 - a size (volume), expressed in litres, notably to evaluate bulkiness
 - a base value (if applicable), expressed in credits and in world currency as well. This base value corresponds to the mean found value for that object in the game world, to be modulated by the actual merchant
 - a wear level, which determines how much the object is worn-out. This is notably useful for armors and weapons

manuals, armor, weapons, flares, lamps.

Treasures


Special Objects
---------------

 - fountains, whose liquids can have different effects 
 - altars
 - thrones, they may have random effects on players willing to experiment with them
 - opal eyes
 - scrolls, that describe spells
 - manuals, that teach interesting matters or not
 
 

Object Quality
--------------

 - bronze, iron, steel
 

Object Wear
-----------

The wear of each object is described by a `Maximum Wear Level` (MWL) and a `Current Wear Level` (CWL). CWL must be in the [0;MWL] range.

The wear percentage is defined to be equal to ``CWL/MWL``.

+-----------------+--------------+--------+----------+
| Wear Percentage | Wear State	 | Usable | Reparable|
+=================+==============+========+==========+
| 0%        	  | New  		 | Yes    | No  	 |
+-----------------+--------------+--------+----------+
| ]0%;25%[  	  | Lightly Used | Yes    | Yes 	 |
+-----------------+--------------+--------+----------+
| [25%;55%[  	  | Used     	 | Yes    | Yes 	 |
+-----------------+--------------+--------+----------+
| [55%;85%[  	  | Worn-out   	 | Yes    | Yes 	 |
+-----------------+--------------+--------+----------+
| [85%;100%[  	  | Broken		 | No	  | Yes 	 |
+-----------------+--------------+--------+----------+
| 100%            | Unreparable  | No	  | No  	 |
+-----------------+--------------+--------+----------+


Each time an object is used, its wear increases by an amount which depends on the action and on its context. For example, when a dagger is used to stab a foe, the wear of the dagger is increased due to the stabbing (action) and to the resistance of the armor of the opponent (context). If it is a chain mail, the dagger may be worn sooner than if the foe had no armor.

Reciprocally, any armor would see its wear increase due to the stabbing.

When an object becomes broken, it cannot operate normally. For instance a weapon would deal little or no damage, an armor would protect a little or not at all. However these broken objects can still be used, thus they can reach an unreparable state.


Managing the wear of equipments can be an interesting gameplay element at first, but in most cases it should not be a constant concern for the player.

To alleviate when appropriate this constraint, object with significant `Maximum Wear Level` and/or relevant repair spells and/or cheap tinkers could be introduced later in the game.

Wear is managed in class_Object.erl_ and tested in class_Object_test.erl_.

Exchanging Goods
================

Transactions.


Barter
------

Purchase
--------



Money & Currency
----------------

In Orge, all the monetary values are internally manipulated as a unique unit, the *credit* (abbreviation: ``C``). Depending on the setting, a credit can be mapped to dollars, gold coins, martian credit, etc.

As Orge focuses primarily on fantasy settings, here is the default mapping:

   ================ ===================== ========================= ================  
   Name of currency Currency Abbreviation Value In Smaller Currency Value in Credits  
   ================ ===================== ========================= ================  
   Copper Coin      cc                    N/A						1				  
   Silver Coin      sc                    1 sc = 20 cc  			20  			  
   Gold Coin [#]_   gc                    1 gc = 50 sc 				1000 			  
   ================ ===================== ========================= ================  

.. [#] Gold, compared to copper and even silver, is quite rare in Orge. 

.. Hint:: Usually making the change is a gameplay killer (since it is too cumbersome and fundamentally uninteresting), thus Orge sees it as a technical detail that can be abstracted away, and no monetary exchange will raise change issues.


Social Organizations
====================


Guilds
------

A lot of guilds are geared towards specific sectors of activity, and therefore correspond quite closely to character classes.


Thieves'Guild
.............

One of the most proeminent and successful guilds: almost every town has it local thieves headquarters, although it is in generally very well concealed from the rest of the population.


Brotherhood Of St.Urquhart
..........................

This monastic community lives apart from secular society, usually in rather large complex of buildings around an abbey. Monks of St.Urquhart are reputed for their hand-to-hand combat technics, and for their focus on group combat.


Order Of Aasgard Knights
........................

Often set on the borders of town or in the wilderness, they take take the form of commanderies. Around an often well-fortified  manor are dispatched a series of stables, training yards, houses and fields.


Circle Of Arcane Rulers
.......................

This ancient guild has for particularity to gather all schoolds of wizardry, although they are known to be hardly able to live together. This guild tends to decline, due to the progress of organizations dedicated to specific schools of magic.

The premises of Arcane Rulers are often near the center of towns, in beautiful buildings, but lacking of repairs, if not already in noticeable decay.

A large number of local circles still have a functional teleportation portal.


Places
======


Towns
-----

These are the place where most commodities for characters in need can be found:

 - taverns offers drinks and meeting places, for example for over-the-counter quests, news exchanges or unofficial recruitment
 
 - banks allow to store character possessions relatively safely 
 
 - hospitals and clinics help restoring health and recovering from wounds and diseases
 
 - inns offer a place to sleep quietly, and to let party members left behind (not selected for a given action)
 
 - city hall and information panels give access to public services and offers (ex: openly published job offers)

 - stores offer thematically sorted goods, with often a good choice; merchants are often difficult in bargaining
 
 - markets are ideal for haggling second-hand properties, if one does not fear to have its purse stolen

 - mainstream guilds have local premises, secret or not, in most towns
 
 - religious places, like churches, temples, etc., help accommodating with one's deity, if any
 
 
Towns are also a place of choice for *safe* social interactions, as combats are expected to be banned there.



Military Installations
----------------------

 - outposts
 - castles
 - citadels
 



Movement & Exploration
======================


Cartography
-----------

Unless specific measures are taken (use of map, compass, magical item, mapping spell, etc.), nothing special assists the player in finding directions except other in-world elements, such as helpful creatures and signposts.

As there are basically no auto-mapper, the player has to rely on its sense of direction and/or its own mapping onto graph paper.

Should an auto-mapper be provided, it would have to include a "fog of war" feature.
 

Game Ingredients 
================


 - mazes (ex: where is the exit to the surface ?)
 - riddles
 - puzzles (ex: zombi chess against the Evil Necromancer)
 - police investigation (ex: who kill King Fleck ?)
 - military strategy (ex: cutting the supplies of an army)
 - military tactics (ex : organizing a raid)
 
 
Quests
======

Dramatic tension can be increased by quests whose success is dictated by achievements within strict time limits.

They are rewarded by:

 - plot progress (milestone unlocked)
 - gain of experience points
 - wealth gain, monetary (gold) and/or not (jewels, etc.)
 - special objects (ex: magical ones)
 - help (ex: powerful NPC joining the main character)
 - special scenes and musics

Basic quest examples:

 - free a character and escort him back to a given point before given time-based event	
 - capture a given boss, too well-guarded for the brute-force raid (infiltration needed)
 - infiltrate given secret society
 
 
A balance has to be found between adventure, action and role-play.



Audio
=====

A huge part of player immersion can come from the in-game audio environment. 

With Orge it is composed classically of:

 - a set of background musics, chosen to match the ambiance of a location or quest events
 
 - special effects, to "enhance the player experience", as marketing people say 
 
 
We believe these special effects are essential, and should play at multiple levels. 

The first level consists on sounds that directly illustrate the action. This includes all special effects like footsteps, door openings, weapon colliding,  grunts of monsters lurking out of sight, heart beating, etc.

A second level corresponds to sounds that generate indistinct threat and  dramatic tension, like an ominous chord progression, or that suggest the ennemy is moving forward, plotting against the player and gathering its forces.

The game should be hardly playable with muted sound.




Multiplayer
===========

Player versus Player.


User-Generated Content
======================

Playing in one's virtual world is pleasing, but being able to add some elements of its own is often considered still more enjoyable.

For the ruler of a virtual world, user-generated content is probably the most interesting way of expanding the game and renewing it. Thus this should be encouraged, beginning from the release to the public of some of the tools, editors, documentation, resources (if the license allows it) that were gathered for in-house world creation.




Player versus Player.


A More Formal Modelling
=======================

This section describes more precisely how the main concepts used by Orge are modelled.

 - Element
 
   - ActiveElement
   
     - Creature: corresponds to all beings, monsters or characters
	 
	   - belongs to a Species
	   - Has an age and a life expectancy at births
	   - Has a gender
	   - has an Inventory
       - Experience Gain if killed by the player
       - Links to possible completion of quest element
	   - onKilled
	   - onSurrender (if appropriate)
	   
     - Clock: trigger based on (simulation) time	   
   - PassiveElement
   
     - Object: corresponds to all  
	 
	   - onPickUp: might trigger actions (ex: raise in statistics, curse, traps, etc.)
	   - onDrop
	   - onUse (if appropriate)
	   - onUseWith (if appropriate)
	   - contains (if appropriate)
	   
     - Challenge: corresponds to all kind of (generally rewarded) actions
	 
	   - Quest: composed of set of Challenges (C1, C2, .., Cn), whose overall completion is defined with regard to the ones of challenges. For example, a quest could be completed iff C1 and C2 are completed, or if C3 is completed
	   
     - Area: an in-game portion (horizontal plane surface) of the map. It can defined as a (convex or concave) polygon, or as a simple disc
	 
	   - RespawnArea: an area in which a given set of kinds of creature appears at a given rate. Maximum populations can be capped. Ex: ``[ {Troll, ]`` 	   
   

.. Note:: Many elements passive by nature (ex: a challenge, an area, etc.) can be nevertheless turned into active elements on a per-instance basis, if associated to a given Clock object.





Orge Implementation
===================


Overview
--------

The Orge game system has been implemented by the `OSDL project <http://osdl.sourceforge.net>`_. This codebase is meant to power multi-player games, notably in the context of persistent worlds (MMORPG). 

Orge has been implemented in the `Erlang <http://www.erlang.org>`_ language. In a few worlds, Erlang has been chosen for:
 
 - its scalability and support for massive concurrency
 - its fault-tolerance abilities
 - its high-level orientation
 - the in-place incremental hot code reload it features
 
The codebase is released through the GPL license, as stated in `License for the game implementation`_.

The full Orge codebase can be browsed from `our SVN repository <http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/>`_.

The Orge implementation is based on the services provided by the OSDL sister project, the `Ceylan project <http://ceylan.sourceforge.net>`_. Ceylan focuses on generic features, whereas OSDL concentrates on interactive, multimedia, game-related applications.


Technically
-----------

Each and every class modelling a game element should be able to send traces, thus should inherit from the ``TraceEmitter`` class.


License
=======
This document describes a game system. It includes a model and an implementation of it.


License for the game model (a.k.a. rules)
-----------------------------------------

The game model is released under a `Creative Commons <http://creativecommons.org>`_ license whose name is *Attribution Noncommercial Share Alike 3.0*, also known as *by-nc-sa*. 


vv.. image:: ../../../../images/by-nc-sa-3-0.png
   :target: http://creativecommons.org/licenses/by-nc-sa/3.0/
   :align: center
   :scale: 100


This license lets you remix, tweak, and build upon this work non-commercially, as long as you credit us and license your new creations under the identical terms.

You can thus download, redistribute, translate, make remixes and produce new systems and games based on this work, as long as you mention us and link back to us, but you cannot use this work commercially.

All new work based on yours will carry the same license, so any derivatives will also be non-commercial in nature.

Any of the above conditions can be waived if you get permission from the copyright holder.

See also a summary of `what can be done with the game model <http://creativecommons.org/licenses/by-nc-sa/3.0//>`_.


License for the game implementation
-----------------------------------

The Orge implementation is released under the `GNU General Public License <http://www.gnu.org/licenses/gpl.html>`_ (GPL), version 3 or any later version.


Footnotes
---------



Citations
---------



Random Ideas
============

 - once authorisation has been obtained from the controlling players, reinject in-game some characters having been played as NPC or monsters
 
 
 
Bibliography
============


Other well-known game systems
-----------------------------

 - Castles & Crusades, `A Guide and Rules System for Fantasy Roleplaying <http://www.trolllord.com/newsite/downloads/pdfs/cnc_qs.pdf>`_ [PDF]
 
 - `Spiderweb Software <http://www.spiderwebsoftware.com/>`_, notably their latest games, `Avernum 5 <http://www.avernum.com/avernum5/index.html>`_ (see `these hints <http://www.ironycentral.com/cgi-bin/ubb/ubb/ultimatebb.php?ubb=get_topic;f=23;t=000013>`_ about its game system) and `Geneforge 4 <http://www.spiderwebsoftware.com/geneforge4/index.html>`_
 
 - GURPS
 
 - FUDGE
 
 - Star Command: see `1 <http://www.the-underdogs.info/game.php?id=1040>`_, `2 <http://www.mobygames.com/game/star-command>`_


 
 - `Diablo II <http://www.battle.net/diablo2exp/>`_



References & Sources
--------------------

 - `Wikipedia article <http://en.wikipedia.org/wiki/Role-playing_game_system>`_ about role-playing game systems

 - `Hirinkaël document about modifier management <../../../club/game/numericBook/ThresholdSimulationSystem.pdf>`_ (in French)

 - An exciting series of articles of Matt Barton, `The History of Computer Role-Playing Games <http://www.gamasutra.com/features/20070223a/barton_01.shtml>`_

 - *Dungeons & Desktops, The History of Computer Role-Playing Games*, the full book of the same Matt Barton. ISBN : 978-1-56881-411-7. Surely a must-read. See also the corresponding `extract <http://www.gamasutra.com/view/feature/3674/book_extract_dungeons_and_.php>`_

 - Wikipedia article about `MUD <http://en.wikipedia.org/wiki/MUD>`_
 
 
:raw-latex:`\pagebreak`

Appendices
==========

.. contents:: List of appendices
	:depth: 1

:raw-latex:`\pagebreak`

.. include:: Orge-technical-manual.rst


:raw-latex:`\pagebreak`

.. include:: Orge-orders-of-magnitude.rst


:raw-latex:`\pagebreak`

.. include:: Orge-glossary.rst


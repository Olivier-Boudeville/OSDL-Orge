
:raw-latex:`\pagebreak`



Orge Conventions
================


Some RPG-related Definitions First
----------------------------------

These following central terms will be used everywhere in this document.
See also the `Orge Glossary`_ for Orge-specific terms.



Types of games
..............


Table-top Game
	This is the classical `role-playing game <http://en.wikipedia.org/wiki/Role-playing_game>`_, also known as *pen and paper* game, where participants meet in real life with little or no technical help (only documentation, dice, a cardboard screen, etc.).   

RPG Video Game, or Computer Role-Playing Game (`CRPG <http://en.wikipedia.org/wiki/Computer_role-playing_game>`_)
	This was, originally, mostly a translation attempt of table-top games into a video game, even if it resulted eventually in a rather different game genre: roleplay disappeared a lot and let its place too often to `levelling <http://en.wikipedia.org/wiki/Level_up#Level-based_progression>`_, with simplistic scenarios involving stereotypical scenarios (door/monster/loot, repeat).
	
Interactive Fiction (`IF <http://en.wikipedia.org/wiki/Interactive_fiction>`_)
	This refers to all software that simulate environments and that could be understood as well as literary narratives as computer games.

	

Roles Of Game Participants
..........................


System Designer (SD)
	This is the author of the Game System, i.e. of the generic conventions and rules that are to be applied to all the kinds of supported games. Typically here the SD is us, the Orge authors, writers of the document you are reading. 

Scenario Writer (SW)
	This is the demiurge and story-writer, who defines the world in which the action will take place, with its associated plots, quests, characters, etc. Regarding the basic rules of interaction, he will rely directly on the Game System provided by the SD, so that he can focus on the core of his subject, the story. Note that the SW defines an interactive story only once, but it can result in any number of game instances based on that story.

Game Master (GM)
	This is the one that allows the players to interact with the game world, in the context of a story. His role is to animate a game instance based on a story created by a SW. He is the actual story-teller.

Player 
	The player is interpreting the role of a character involved in the world recreated by the GM (*roleplay*). 


Of course one can cumulate multiple game roles: for example some GM prefer pre-cooked scenarios, whereas others are SW as well. 	

A link between SW and GM exists for CRPG as well, since the tools for scenario creation (editors for characters, objects, quests, maps, etc.) have to be compatible with the actual RPG engine being used.

	
	
Characters
..........

		
Character
	Generally speaking, a Character corresponds to a sentient being, endowed with at least some reason. The definition has to be rather vague, as, beyond the classical men and women, a Character could be as well an alien or an advanced computer making use of artifical intelligence. Creatures deemed not smart enough to have elaborate feelings are not characters, depending on their roles they are sometimes designated as monsters.   
	
Player Character (PC)
	A PC is a Character that is interpreted by a player, who assumes its role and takes control over many of his actions.
	
Non-Player Character (NPC)
	A NPC is a Character that is not interpreted by a player, it has thus to be managed by the GM.



Wrapping around
...............


Usually there is exactly one Game Master, some players, each interpreting only one particular Player Character at a time [#]_, and a large number of beings, including monsters and NPC.

.. [#] Some games allow a player to have multiple accounts, with one or more characters per account, and at most one active character per account at any time.


A game instance may or may not be split into sessions.

In the case of table-top games, a *game* follows a story line made usually of a set of *campaigns*, which are made of a series of *adventures*, which, for convenience reasons (due to durations often above a few hours) are broken into a series of *game sessions*.

For RPG video games, either the world instance is created especially for a group of players, and then the world exists only while they are playing (be it a single player or multiplayer game), or the world instance lives continuously, i.e. is persistent, be there players or not, in the case of a `MMORPG <http://en.wikipedia.org/wiki/MMORPG>`_ (*Massively Multiplayer Online Role-Playing Game*).

In both table-top and video games, an adventure includes generally a set of *quests*, or missions.

All participants are expected to create and follow *collaboratively* a story based on roleplaying.




Other Conventions
.................


Non Orge-Specific Terms
_______________________


AI
	Artificial Intelligence, notably creatures controlled by the game engine rather than by a real player.
	
Bonus
	Positive modifier.
	
Buff
	Temporary beneficial effect on a character. See also this `Wikipedia article <http://en.wikipedia.org/wiki/Buff_(computer_gaming)>`_. In the context of Orge, a Buff can come from two sources: drugs and magic.
	
iff
	It means `If And Only If <http://en.wikipedia.org/wiki/If_and_only_if>`_. ``A iff B`` means either both statements are true or both are false. 

Malus
	Negative modifier; penalty.
	
N/A
	It means ``Non Applicable``. 

Party
	A group of characters, often complementary, working as a team.
	
Singleton
	A character who does not belong to a party, adventuring on its own.
		

See `Orge Glossary: Main Acronyms & Terms`_ for Orge-specific terms.




Units & Orders of Magnitude
___________________________


Units are to be expressed mostly in the `international system (SI) of units <http://en.wikipedia.org/wiki/SI_base_unit>`_ and its `derived units <http://en.wikipedia.org/wiki/SI_derived_unit>`_. 

This includes:

================== ====== =================== =============================== 
Unit name          Symbol Associated Quantity Common multiples
================== ====== =================== =============================== 
meter 	           m      length              kilometer (km), centimeter (cm)
cubic decimeter    dm^3   volume              cubic meter (1 m^3 = 1000 dm^3), litre (1L = 1 dm^3) 
kilogram           kg     mass                gram (1 kg = 1000 g), metric ton (1t = 1000 kg)
second 	           s      time                minute, hour, day, year, century
meter per second   m/s    speed               km/h (1 m/s = 3.6 km/h)
degree Celsius     °C     temperature         (none)
newton 	           N      force               kilonewtons (1 kN = 1 000 N)
joule              J      energy, heat        kilojoule (1 kJ = 1 000 J)
candela	           cd     luminous intensity  (none)
pascal             Pa     pressure            kilopascal (1 kPa = 1000 Pa)
================== ====== =================== =============================== 


To better evaluate physical values, please refer to the examples listed in the `Some scales and orders of magnitude`_ section.




General Operating Principle
---------------------------

The role of a game engine like Orge is mostly to *emulate* a Game Master, during a game session with real (human) player(s). 

The task includes:

 - describing to the players the world surrounding their characters
 - offering the players the means to interpret their character
 - resolving the corresponding actions 
 - evaluating the state and behaviour of all game elements besides the ones controlled by the players


The GM task can be described also as an interactive simulator operating on game elements chosen, defined and arranged by the SW, based on the modelling provided by the SD. Thus the vocabulary of simulation is often used in this document. Games are just a specific kind of simulation indeed.

.. Note:: Although there are presumably only a few CRPG (actually we do not know any of such games) that offer to a human player the technical possibility of being the GM instead of controlling a character, this possibility could be interesting and thus could be supported in the future by the Orge implementation.  

Emulating the GM corresponds to the server-side computations of a game: we are dealing here only with models, without taking account input management, rendering nor other concerns. This is the place where we evaluate the game world and update its state (based on previous states and all inputs). 

Thus, for a MMORPG, Orge is to be found running on the datacenter hosted by the game operator.

For a (non-persistent) multiplayer game, Orge would be running on the game server(s).

For a single player game, Orge could be run on the computer of the player. It would be still running logically as a server, even though, technically, it could be in the same process, with no network I/O.


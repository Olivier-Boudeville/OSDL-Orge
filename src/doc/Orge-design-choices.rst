
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

Recurrent Declaration about the Importance of Roleplay
--------------------------------------------------

Each and every game system, after dozens of pages involving equations and algorithms, ends up with the finding that all that matters is storytelling (on the GM side) and roleplaying (on the player side). There will be no exception here.



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




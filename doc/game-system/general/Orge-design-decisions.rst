:raw-latex:`\pagebreak`



Orge Design Decisions
=====================

This is a set of arbitrary choices regarding what kinds of settings are supported by Orge. The Orge game system seeks to be as universal as reasonably achievable.


Video Games versus Table-top Games
----------------------------------

Orge is a game system primarily dedicated to video games.

However it can be used as well for table-top games, since most computations may remain simple thanks to the use of abacuses (which are simplified precomputations to be used graphically).



Realistic versus Epic Setting
-----------------------------

Depending on the interactive story to be told, some actions might be totally impossible or, only, very unlikely (this is a difference of nature). The more unlikely outcomes may happen, the most epic the story will be.

With Orge we prefer to keep a certain level of "epicness", should such word exist. Thus extraordinary actions are always possible (notably thanks to the modifier system we use), even if they are very unlikely.

As too much realism would as well hinder the entertainment, most facts of life that are relatively uninteresting (ex: the need for a character to go regularly to the toilet) have been abstracted out. Others have been considerably alleviated. See also: `Character Needs For Survival and Well-Being`_.



Place of Random
---------------

We chose to introduce some randomness when resolving actions, as it creates a kind of "dramatic tension". It forces oneself to evaluate every possible outcome and to ponder them painfully.

Regarding game content, having it generated based on randomness might be a bit hazardous: roaming through endless self-similar dungeons is not deemed fun enough. Better have a few well-crafted dungeons, authored by real-life designers, rather than infinite dull ones.

Same reasoning applies to random-generated quests, too many "fetch the orb from evil Wizard X" quests would kill most of the fun.

There are relevant areas for randomness in game content tough, like for the terrain generation: well-chosen fractal algorithms lead to realistic landscapes, on which human-generated content can be settled down for the better of both worlds.

Another interesting use of randomness is for the spawning of creatures: even if respawn areas should better be built by hand, they are then populated automatically. Otherwise monster populations would not be replenished, or too much manual tweaking would be necessary. See also: `Respawn`_.

Item randomization [#]_ is also useful.

.. [#] As done in single-player, non-persistent games like `Nethack <http://www.nethack.org/>`_, where, depending on each specific game instance, the appearance of a swirly potion may refer to a polymorph potion, or to a healing one, etc., thus preventing too much of the game content to be affected by spoilers.


A right balance between random encounters and pre-established (ex: scripted) confrontations must be found, as both are needed.



Tone: Tragedy versus Comedy
---------------------------

We preferred letting the game system be, as much as possible, tone-agnostic: the choice of the mode of fiction is left to the scenario writer. See also: `Place of Death`_.



Setting: Medieval, Contemporary, Futuristic, etc.
-------------------------------------------------

The majority of popular MMORPGs are based on traditional fantasy themes, often occurring in an in-game universe comparable to the one introduced by *Dungeons & Dragons*.

Some of them employ hybrid themes that either merge or substitute fantasy elements with those of science fiction, sword and sorcery, post-apocalyptic survival, or crime fiction.

Currently Orge aims more specifically at fantasy settings. Later science-fiction themes could be explored, notably deep-space exploration and warfare. Contemporary stories are maybe a little less attractive.



Party & Singleton
-----------------

In some games the player identifies himself to a single hero (a singleton), whereas on others he controls a team of adventurers, i.e. multiple characters (a party). This is a completely separate concern from the single player/multiplayer design issue.

For the moment Orge focuses mostly on singleton-based games, although team management can be interesting too, when building with long term planification each character so that it complements the others.

A somewhat intermediate setting is having a singleton joined by NPC allies. Although these sidekicks are defined and controlled by the Game Master, this allows to micro-manage more than one character, and to benefit from a larget set of available skills. This may result also in increased team building, social interactions, negotiations, etc.

Moreover, as these wingmen are preset (predefined by the Game Master), they can help developping the plot with a more linear - thus narratively easier to design - story-line.

Should wingmen be found limiting too much the possible outcomes, they can be given different and incompatible personalities and motivations. Then players are given back some power of decision, since they have to select carefully which preset characters are to be included in their party, and to choose their own trade-offs. Mistakes could lead to dissensions in the party, splits, fights or betrayals at critical moments.

An alternative or complementary approach to these NPC allies could be to have *familiars*, in general pets. They can go with the main character, help him in various tasks (including combats), and be upgraded. Familiars are generally tamed animals (ex: wolves) or summoned creatures (ex: sarcastic and irritating minor demons). They tend not to be directly integrated to the story-line.



Roleplay
--------


Recurrent Declaration about the Importance of Roleplay
......................................................

Each and every game system, after dozens of pages involving equations and algorithms, ends up with the finding that all that matters is storytelling (on the GM side) and roleplaying (on the player side). There will be no exception here.


Ways of Encouraging Good Roleplay
.................................


Roleplay is one of the corner stones of a successful online RPG.

To help the development of roleplay, players are encouraged to customize their characters, notably thanks to a textual description.

Roleplay should not be confused with social interactions, since there is a difference between immersive games and instant messengers. We want here to promote contextual exchanges, based mostly on in-game events.

In Orge, some game facilities are provided for that:

  - real-time IRC-like internal chat system, for interlocutors able to speak directly (i.e. if in hearing range). Speech will disappear soon after having being emitted, and cannot be logged. Text size will reflect tone and volume. Poor hearing circumstances will yield to the hearing of corrupted text. The system encourages questions to be asked with an interactively-defined set of possible answers, so that communications with players and NPC can integrate more seamlessly, and to allow for limited automatic translations between player languages for the most usual exchanges. In the future, the system may make some steps towards simplified smart free-text parsers

  - bulletin board systems and posts, for asynchronous unreliable long-distance communication (couriers are expensive, and can be intercepted or happen to have no ethics) [#]_

  - a guild system, to structure the profession-related relationships

  - a stage direction system, allowing the player to specify attitudes and actions that cannot be expressed directly by the game system, a bit like e-motes. For instance: *Gurg leans on the bar, out of breath and, apparently, exhausted*.


.. [#] A problem is that players have often out-of-band communication solutions, either thanks to free direct speech being allowed by the GM on a table-top settings or by using networked chat systems, forums and wikis for the computerized counterpart games. The former seems to be less damaging to the story, and remains more under the GM control, than the latter, which may be partially hindered by per-client randomization (ex: through all game-provided communication solutions - even the in-game chat system - if player P1 mentions a character who is named in his context Foo, whereas the same character is always referred to Bar in the context of P2, then Foo will be automatically rewritten as Bar by P2's client).




An additional way of promoting roleplay is to delegate it, at least partially, to a real-life (human) Game Master, whose role is to better adapt the game's reactions to the acting effort of players in the context of an adventure. For instance, this GM could award experience bonuses for good roleplay, or improve the pre-scripted NPC behaviours in the face of the actual dialogs.

This requires the Orge system to support, beyond the Player role, the Game Master role, and to provide specific tools for it, like the ability for a GM to take control of one or more creatures (NPC, monsters, etc.) appropriately.

The Game Master role of promoting roleplay could be also distributed among all players: during a game session, each player could have an experience pool that would be slowly filled by the game engine, and each player could use these accumulated points to reward those among the other players that he found developing good roleplay. Of course self-donation and too obvious exchanges of friendly services would not be allowed.

Other measures can be taken to further enhance the roleplay, like making mandatory the formation of groups of players, in the face of adversity, either because the opponents are too strong or because the challenges require specialized complementary skills (ex: detector, tank, damage dealer, healer, buffer, etc.). Group spells (ex: team buffs) help there too.




Time
----


Passage of time
...............


The passage of time is another major game element, directly linked to the `Place of Death`_, to `Aging`_ and, to a lesser extent, to `Object Wear`_. The time cannot be stopped, and, although it could be set differently, it flows quite fast in Orge, even faster than in real life.

The game world is persistent, but most beings are relatively short-lived, and players have to find trade-offs between youth and experience.


Turn-Based versus Real Time
...........................


With Orge, interactions in general, and especially tactical combat, could be said, to a large extent, turn-based, as they depend more on reflection than on reflexes. Other actions (ex: exploration) are, most of the time, done in real-time.

Life itself is in real-time, and the turn-based approach raises issues in a multiplayer context: while some characters would experience slow-passing time during, for example, combats, others could be able to explore the world at full speed. What if these roaming characters were to interact with characters fighting in their slow "time bubble"?

At the very least, each turn should be bounded in strict time limits, so that interaction time cannot drift too much from the overall game time. For example, all the players involved in a combat would decide privately and concurrently of their actions, which would then be triggered automatically by Orge only at the end of the turn. The end of turn itself occurs either when all players issued their orders or when the maximum duration of a round is reached, whichever deadline comes first. If a player takes too much time to decide his actions, the game will automatically skip to the next turn.

But this would not be enough, as players outside of the action could nevertheless break in and, for example, bring unfairly considerable back-up while an ambush is going on. The general rule of thumb respected by Orge is that, the closer to an interaction a creature will be, the closer to the pace of the interaction its experienced time will flow.

For example, a character joining a combat will have to go through smaller and smaller time bubbles, entering each one resulting on its time flowing more slowly, until it becomes synchronized with the bounded-time turn-based combat. Conversely, a character getting farer from an interaction will have its time accelerated step-by-step. Should time bubbles intersect, the experienced pace in this area will correspond to the one of the slowest time bubble.

Another key point is that, if combats must not be too slow, they must neither be too fast. It is an Orge design decision: interactions should leave a place for careful player planification, instead of frantic action.

With this kind of "relaxed" real-time system, beware to bathroom breaks! See `Time Bubbles`_ for more details.


Quantification
...............

Time is internally managed in rounds, i.e. unsigned integer simulation ticks. Thanks to the time-bubble delay correction algorithm, a general direct relation between user time (wall-clock) and virtual (overall) game time is maintained: this simulated time flows on average 8 times as fast as the user one. Thus a virtual day lasts for 3 hours.



Rules
-----

Role of Rules
.............


Rules are only guidelines, and may be proved wrong on some occasions.

Game Masters can override them when they deem it appropriate, as rules are the servants of the stories, not their masters.


Visibility of Rules
...................

Orge is interface-agnostic: regarding rules, although their detailed outcomes may be given to the player (ex: *Brian used its Dodge skill level 3 but due to a saving throw of 37 failed to avoid the blow whose Hit Bonus was +12%*, etc.), we think that a presumably better (more narrative) way of designing the interface could be *Brian failed to dodge the Mammoth and was crushed by its gigantic foot*.


Collections
-----------

A lot of game elements could be part of a larger thematical set. Once a full collection is gathered and used as a whole, considerable bonuses are provided: the sum is far greater than the parts.

For example, a balanced team of a species with complementary skills respecting a predefined scheme could be more powerful than expected.

Similarly, pieces of armors (ex: helmet, gauntlet, etc.) with an homogeneous theme (Leather, Steel, Adamant, etc.) would result in a more resistant overall armor once united, etc.

Beyond team member and objects, spells and most game elements could follow that rule.



Ending
------

Single player and multiplayer non-persistent games should have at least one successful ending, and more probably numerous ones, with various levels of success and failure, some depending on the goals which were elected by the player during game.

Multiplayer games in a persistent world should have a far increased lifespan, less related to specific stories reaching completion. However all simulated worlds, including MMORPG, will have an actual termination in real life, and it should be preferably brought by a scenarized in-game final fireworks for a memorable and satisfactory ending.

Unethical endings may or may not be discouraged by the game system.

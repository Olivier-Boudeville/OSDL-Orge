 
Interactions
------------

This includes combats, magic, dialogs, etc.


Time Bubbles
............


Principle
_________


In a multiplayer context, when multiple characters are involved in conflicting and/or complex interactions, they often would like to benefit from a tactical turn-based mode of operation, instead of having to use reflexes for real-time actions.

However, the same in-game time being experienced by all, the (potentially very numerous) other players should not be slowed down each time two players are interacting.

The solution used in Orge is to define a set of time bubbles, i.e. circular rings of increasing radius centered on the interaction [#]_. When a creature is located on a given ring, its experienced time will be all the more slowed down that the ring is near the interaction, i.e. that its radius is small. Thus other characters would act at normal speed when far from the interaction, but, should they become closer, would reach, step-by-step (ring by ring), the interaction speed.

.. [#] The center of these concentric rings corresponds to the barycenter of the creatures involved in that interaction.


The interaction speed can be set by the interacting parties: if all agree to accelerate it, then it will be accelerated.

In all cases, after the interaction is over, all creatures will be back to the overall pace *and* to the overall time: the elapsed rounds will be caught up, so that for example lasting interactions cannot be used to delay character aging.  

Time bubbles are of paramount importance in combats.


Pitfalls & Abuses
_________________


Playing with time can lead to unexpected results. Players may hijack the system and use interactions for their side effects, the time deceleration. Let's see what they can do with that!




Intimidation
............

When a creature notices other potentially hostile creatures, it may be scared by this encounter. The reaction of the creature depends on:
 
 - its willpower
 - its mental fatigue
 - on the intimidation factor of the met creatures, which is the sum of the intimidation factors of all the creatures of the group

Should the creature fail the intimidation test, it can suffer from an action penalty the next rounds, or be completely paralyzed by fear.


Discussion
..........

If all interacting parties agree on talking, and if they are able to do so (i.e. they have at least a language in common), various story-dependent dialogs could be offered, but a few built-in options will always be available:

 - ask for truce
 - stay mute
 - propose a deal, a transaction
 - ask for tribute
 - ask to surrender
 - impersonate deity [#]_

.. [#] Reference to Starcommand
 
Tones can be specified also, with following expression built-in modifiers:

 - volume: soft (whispered)/neutral/loud (shouted)
 - pitch: low/neutral/high
 - cordiality: polite/neutral/rude
 - attitude: friendly/neutral/hostile
 - speed: slow/neutral/fast
 - style: monotonous/neutral/stressed
 - accent: popular/neutral/formal
 
Morever some stage direction can be specified by the players, to specify some non-verbal behaviours that could not be conveyed otherwise (ex: *Almaric looks subtly anxious*).

Some languages are not species-specific but profession-specific, for example the cant used by rogues for their communications (street language).


Stealing
........

All characters (NPC or not), including merchants, can be robbed, by stealth (victims of a pick-pocket) or by violence (threaten or murdered).


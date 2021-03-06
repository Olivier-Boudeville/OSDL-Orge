Creature State
--------------

Contrary to characteristics (primary/secondary attributes), which are rather static, there are other attributes that reflect the changing state of a creature.

These *state attributes* are:

  - current age
  - state of mind
  - health and wounds
  - location in virtual world
  - belongings
  - fatigue
  - mana points
  - thirst level
  - hunger level


In terms of state of mind, states of altered consciousness are:

 - *Blessed/Cursed*: the probability of action success, the character probability to hit, damages dealt and resistances are increased/decreased

 - *Shielded/Exposed*: damages inflicted to the character are reduced/increased

 - *Hasted/Slowed*: the character can perform more/less actions per unit of time, notably in combat, where it can gain/loose attacks

 - *Charmed*: some special abilities or spells can partly take control of a character; this state can be hardly concealed to other characters, but the charmed character will act in some way according to the will of its controller, which results often in it attacking its allies

 - *Scared*: a scared character will instantly break combat and try to escape to its best - although terror is often a poor adviser; the character will flee all enemies, fearsome or not

 - *Drunk*: too much alcohol ingested

 - *Drugged*: too much drug smoken or injected (includes poison)

 - *Possessed*: includes hypnosis, sleep-walking (somnambulism) and magic control

 - *Unconscious*: includes knocked-out

 - *Sleeping*

 - *Mad*: includes illness (mental disease)


Note also that the state (and behaviour) of creatures may depend on the context, on the environment they are in: for example, some species may experience morale failure if temperature is too low, or may be well adapted to the dark and thus have a tendency to lurk mostly at night, etc.

This model is declared in ``class_Creature``, with the `creature_fatigue_model` record.

See the `Fatigue`_ section for further details.



Health Modifiers
................

They depend on the overall health of the creature, but also of any wounds it suffers from.

For example, if attempting a long jump, the fact of having a leg sprained implies a decreased probability of success.

In general, wounds have a very significant impact on action outcomes, so one should try to avoid them by all means, and recover from them as soon as possible.


.. include:: Orge-damage-resistance.rst


Creature Characterization
=========================

Not depending on a creature being a humanoid, a sentient being, a NPC, a monster or a PC, it will be described in an uniform way, by the means of *character's statistics* (a.k.a. "stats").


Species
-------

Each creature belongs to a specific species.


Gender
------

Most creatures are either male or female, permanently. This has an impact on attributes.


Name
----

The name is purely a convention (it is *not* a creature identifier), as a creature may not have a name, or may have multiple names, and a given name may be given to multiple creatures.


The name of a character is mostly determined by:

 - its species
 - its gender


For that matter, the Orge implementation provides a tool using a Markov-based algorithm in order to "learn" any language and then be able:

 - to generate words which could likely be part of a given language/word-set
 - to determine the probability that any specified word belongs to that language

Therefore, once the program is trained against dictionaries containing, for example, names for Elven Ladies, a player can either ask the Orge implementation to invent any number of names that would correspond to that language, or specify a name that the player imagined, in order the Orge implementation to validate it. In that context, if actually supplying the name of a male Dwarf, Orge would probably determine that the probability that this name belongs the Elven Ladies word set is too low, and would reject it.

All details are available in the `Orge Languages Management`_ section.



History
-------

To reinforce roleplay, some elements of history could be specified at character creation, especially if this character is not chosen to be young.

To do so, the player will have to enter initially a few lines of free-form text, that will be recorded once for all as the own autobiography of its character, and be considered as a reference to the true events which happened prior to this life in the game.


Then, in a dialog context, the player will be able to share the - subjective, reference-compliant or not - story of its character with others, with little effort (ex: just clicking on the appropriate button). After its creation, this text could be updated to reflect newer events or newer interpretations.

Later on, a more complex interface could be offered to help players writing interesting summaries, maybe thanks to a set of questions.


Persona
-------

To further improve roleplay, to each character a specific persona should be associated, either by the GM or, more often, by the player interpreting that character.

This personality should be compatible with the character's characteristics, including species and history.

Once established, the persona should be summarized in a few sentences that will be entered as part of the character description. This will act as a reminder for the player, and these information will be kept private to the character.

Quite a lot of details are interesting here: the physical description of the character that cannot be conveyed by the graphics (but that can and should be shared), and its motivations, ethics, background, oddities, etc.

Both physical and psychological portraits should be updated from time to time during the character's life, to reflect the consequences of what it experienced.

Life base expectancy, expressed in years, is determined at creature creation.


Belongings
----------

Each AI-controlled creature - character (NPC) or not (monsters and alike) - must have realistical belongings, and this must be reflected by appearance, behaviour and loot.

For example, a swamp lizzard is not expected to wear an armor of plates or to collect silver coins, and if a rogue attacked with slashing damages, once dead the corresponding weapon (ex: a sword) must be found on his body.

However, body and/or belongings might disappear in some cases (ex: the thief jumped in the deep well, or his allies took his weapons before fleeing, etc.).

As a consequence, if a creature - monster or sentient, NPC or player - dies, any other creature - monster or sentient, NPC or player - may take and make use of any belonging the dead creature was carrying.

Therefore some equipments should not be carried when fighting against some monsters, lest they win the confrontation and gain access to weapons that make them very, very tough.

Possessions can be dropped, concealed, stored in one's house or put in a bank account, instead of being carried. No totally reliable solution exists, though: objects can be found or robbed from all places, including banks. Walking along with friends offers some sense of security.

The temptation of wearing nothing but the bare minimum (so that nothing is lost in case of a bad encounter) is not really an option: no resistance against damage or low temperature would be enjoyed, naked or lightly clothed characters are usually rejected by most social standards, etc.

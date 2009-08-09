

Creature State
--------------

Contrary to characteristics (primary/ssecondary attributes), which are rather static, there are other attributes that reflect the changing state of a creature.

These *state attributes* are:

  - current age
  - health and wounds
  - location in virtual world
  - belongings
  - fatigue
  - mana points
  - thirst level
  - hunger level

States of altered consciousness are:

 - Blessed/Cursed: the character probability to hit and damaged dealt are increased/decreased
 
 - Shielded/Exposed: damages inflicted to the character are reduced/increased     
 - Hasted/Slowed: the character can perform more/less actions per unit of time, notably in combat, where it can gain/loose attacks
 
 - Charmed: some special abilities or spells can partly take control of a character; this state can be hardly concealed to other characters, but the charmed character will act in some way according to the will of its controller, which results often in it attacking its allies
 
 - Scared: a scared character will instantly break combat and try to escape to its best - although terror is often a poor adviser; the character will flee all enemies, fearsome or not  
 
 - Drunk: too much alcohol ingested
 
 - Drug: too much drug smoken or injected (includes poison)
 
 - Possessed: includes hypnosis, sleep-walking (somnambulism) and magic control

 - Unconscious: includes knocked-out

 - Sleeping 
 
 - Mad: includes illness (mental disease)


Note also that the state (and behaviour) of creatures may depend on the context, on the environment they are in: for example, some species may experience morale failure if temperature is too low, or may be well adapted to the dark and thus have a tendency to lurk mostly at night, etc.
 
This model is declared in class_Creature_, with the `creature_fatigue_model` record.

See the `Fatigue`_ section for further details.



Health Modifiers
................

They depend on the overall health of the creature, but also of any wounds it suffers from.

For example, if attempting a long jump, the fact of having a leg sprained implies a decreased probability of success.

In general, wounds have a huge impact on action outcomes, so one should try to recover from them as soon as possible.


.. include:: Orge-damage-resistance.rst


Creature Characterization
=========================

Not depending on a creature being a humanoid, a sentient being, a NPC, a monster or a PC, it will be described in an uniform way, by the means of *character's statistics* (a.k.a. "stats").


Gender
------

Most creatures are either male or female, permanently. This has an impact on attributes.


History
-------

To reinforce roleplay, some elements of history could be specified at character creation, especially if this character is not chosen to be young.

To do so, the player will have to enter a few lines of free-form text, that will be recorded as the own autobiography of its character. Then, in a dialog context, the player will be able to share the - subjective - story of its character with others, with little effort (ex: just clicking on the appropriate button). After its creation, the text could be updated to reflect newer events. 

Later on, a more complex interface could be offered to help players writing interesting summaries, maybe thanks to a set of questions.


Persona
-------

To further improve roleplay, to each character a specific persona should be associated, either by the GM or, more often, by the player interpreting that character.

This personality should be compatible with the character's characteristics, including species and history. 

Once established, the persona should be summarized in a few sentences that will be entered as part of the character description. This will act as a reminder for the player, and these informations will be kept private to the character.

Quite a lot of details are interesting here: the physical description of the character that cannot be conveyed by the graphics, and its motivations, ethics, background, oddities, etc.

Both physical and psychological portraits should be updated from time to time during the character's life, to reflect the consequences of what it experienced.  
Life base expectancy, expressed in years, is determined at creature creation.


Belongings
----------

Each AI-controlled creature - character (NPC) or not (monsters and alike) must have realistical belongings, and this must be reflected by appearance, behaviour and loot.

For example, a swamp lizzard is not expected to wear an armor of plates or to collect silver coins, and if a rogue attacked with slashing damages, once dead the corresponding weapon (ex: a sword) must be found on his body.

However body and belongings might disappear in some cases (ex: the thief jumped in the deep well).

As a consequence, if a creature - monster or sentient, NPC or player - dies, any other creature - monster or sentient, NPC or player - may take and make use of any belonging the dead creature was carrying.

Therefore some equipments should not be carried when fighting against some monsters, lest they win the confrontation and gain access to weapons that make them very, very tough.

Possessions can be dropped, concealed, stored in one's house or put in a bank account, instead of being carried. No totally reliable solution exists, though: objects can be found or robbed from all places. Walking along with friends offers some kind of security.

The temptation of wearing nothing but the bare minimum (so that nothing is lost in case of a bad encounter) is not really an option: no resistance against damage or low temperature would be enjoyed, naked or lightly clothed characters are usually rejected by most social standards, etc.




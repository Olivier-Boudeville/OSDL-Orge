

Creature State
--------------

Contrary to characteristics (primary/ssecondary attributes), which are rather static, there are other attributes that reflect the changing state of a creature.

These *state attributes* are:

  - current age
  - health
  - location in virtual world
  - belongings

Other states of altered consciousness are:

 - Blessed/Cursed: the character probability to hit and damaged dealt are increased/decreased
 - Shielded/Exposed: damages inflicted to the character are reduced/increased     
 - Hasted/Slowed: the character can perform more/less actions per unit of time, notably in combat, where it can gain/loose attacks
 - Charmed: some special abilities or spells can partly take control of a character; this state can be hardly concealed to other characters, but the charmed character will act in some way according to the will of its controller, which results often in it attacking its allies
 - Scared: a scared character will instantly break combat and try to escape to its best - although terror is often a poor adviser; the character will flee all enemies, fearsome or not   
 
This model is declared in class_Creature_, with the `creature_fatigue_model` record.

See the `Fatigue`_ section for further details.


Health Modifiers

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




Life base expectancy, in years


Belongings
----------

Each AI-controlled creature - character (NPC) or not (monsters and alike) must have realistical belongings, and this must be reflected by appearance, behaviour and loot.

For example, a swamp lizzard is not expected to wear an armor of plates or to collect silver coins, and if a rogue attacked with slashing damages, once dead the corresponding weapon (ex: a sword) must be found on his body.




 

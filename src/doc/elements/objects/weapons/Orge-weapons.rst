
Weapons 
-------

Weapons are used in armed combat, as opposed to unarmed combat like some martial arts, boxing, wrestling, etc.

Weapons can be used in close combat and/or ranged combat. For example, a knife could be used to stab, or can be thrown.

Close combat weapons are further split into:

 - melee weapons, like swords
 - pole weapons, like spears

There are weapons by purpose (ex: a sword) or by usage (ex: a pickaxe).

All weapons - even for close combat - have an attack range. For example an halberd allows for a larger range than a dagger.

The controller of a creature can select which creature is to be targeted, but then Orge will determine which creature(s) is/are hit, which may or may not be the desired result (ex: friendly fire can happen). 

Most weapons can be used different ways, to throw, stab, bash, slash, etc, called *attack style*.

This impacts notably on:

	- the rate of attacks
	- the probability to hit 
	- the probability to defend (block/dodge/parry)
	- the inflicted damages
	- the wear of the attacker weapon
	- the wear of the defender armor
	
All weapons, depending on the attack style, inflict at least one specific kind of damage. For example, a Sword can only be used for chopping, and this deals Slashing Damages.

Some weapons support multiple attack styles. For example, an Halberd can be used to perform three different attacks, each with specific characteristics.

Moreover, depending on the weapon and on the attack style, damages will be either inflicted to specific targets (usually one creature; ex: with a sword) or will have an area of effect (which may include zero to any number of creatures; ex: with a fireball). 



Weapon Repository
.................


The Orge built-in weapons are represented in following tree, knowing that they are sorted according to their main attack style:

..  ddd:raw-html:`<img src="weapon-tree-negated.png"></img>`
..  ggg:raw-latex:`\includegraphics[scale=0.75]{weapon-tree.png}`


For melee weapons, following attributes are used:

  - Range: the striking distance, when used as a melee weapon. Ex: a knife has a lower range than an Halberd. Value in `{hand-to-hand,melee,pole}`
  - Weight: the weight of that weapon, in kilograms
  - Bulkiness: the volume that have to be carried for that weapon (in liters)
  - Base Value: the average cost of production of this weapon (in Credits)
  - Ready Duration: the duration needed to switch this weapon from carried to ready to attack and defend
  - Intimidation Factor: the psychological impact on an average foe seeing its opponent using that weapon
  - Attack Styles: the list of attack styles supported by this weapon; for each attack style, following attributes are distinguised:
  
	- Attack Duration: the number of round this attack style implies. Ex: piercing with a Katana is quicker than slashing with an Halberd
	- Damage Type: the type of damage inflicted by this attack style 
	- Base Damage: the amount of base damage of various types dealt when using this attack style. Ex: stabbing with a poisoned dagger can deal simultaneously Slashing and Poison damages
	- Defense Factor: the effect of this attack style on defense. Ex: using an Halberd as Spike (Piercing) allows to maintain opponents at some safe distance, whereas once the Halberd has been used as a Blade (Slashing), the character is quite exposed until it withdrew its Halberd in its waiting position  


For range weapons, following attributes are used:

  - Range: the striking distance, in meters, when used as a ranged weapon. Ex: a Long Bow has a greater range than a Short Bow
  - Reload Time: duration, in milliseconds, needed to have the next ammunition ready when one has been fired, *supposing the magazine is not empty*
  - Weight: the weight of that weapon, in kilograms
  - Bulkiness: the volume that have to be carried for that weapon (in liters)
  - Base Value: the average cost of production of this weapon (in Credits)
  - Ready Duration: the duration needed to switch this weapon from carried to ready to attack and defend
  - Intimidation Factor: the psychological impact on an average foe seeing its opponent using that weapon
  - Attack Styles: the list of attack styles supported by this weapon; for each attack style, following attributes are distinguised:
  
	- Attack Duration: the number of round this attack style implies. Ex: piercing with a Katana is quicker than slashing with an Halberd
	- Damage Type: the type of damage inflicted by this attack style 
	- Base Damage: the amount of base damage of various types dealt when using this attack style. Ex: stabbing with a poisoned dagger can deal simultaneously Slashing and Poison damages
	- Defense Factor: the effect of this attack style on defense. Ex: using an Halberd as Spike (Piercing) allows to maintain opponents at some safe distance, whereas once the Halberd has been used as a Blade (Slashing), the character is quite exposed until it withdrew its Halberd in its waiting position  



axe: when chopping, deals Slashing Damage
longsword: when chopping, deals Slashing Damage
scimitar: when chopping, deals Slashing Damage

rapier: when used, deals Piercing Damage
katana: when used, deals Piercing Damage
dagger: when used, deals Piercing Damage


Halberd 
_______

Also known as Halbert or Swiss Voulge, it is a two-handed pole weapon.
This weapon is heavy and quite cumbersome (1.5 to 1.8 meters long), but rather cheap to produce and very versatile in battle: it can be used as an Axe (thanks to its blade), a Spike (as topped with a spike mounted on a long shaft) and a Hook (on the back side of the axe blade), very useful for grappling mounted combatants.


+--------------+-------------+-----------------------------+---------------+
| Attack Style | Attack Rate | Base Damage                 | Defense Factor|
+==============+=============+=============================+===============+
| Slash        |             | SLS                         |               |
+--------------+-------------+-----------------------------+---------------+
| Pierce       |             | SLS                         |               |
+--------------+-------------+-----------------------------+---------------+
| Grapple      |             | SLS, PRC                    |               |
+--------------+-------------+-----------------------------+---------------+



To be added:
  
  - Whip and Cat-o-nine-tails
  - Blow-pipe: different Darts (ex: poisoned, paralyzing)
  - Bow: different Arrows
  - Crossbow: different Bolts
  - Blunt (most can be light or heavy): Hammer, Hand Axe, Flail, Ball and Chain
  - Crossbow: Hand Crossbow, Light Crossbow
  - Stick: Rod, Cudgel, Quarterstaff, Tonfa, Blackjack, Wand
  - Slash: Sickle, Sabre, Bastard Sword
  - Sabre: Cutlass
  - Spear: Long Spear, Voulge
  - Lance: Long Lance
  - Pierce: Pick, Fork, Trident, Sleeve Tangler, Gaff, Crowbill Hammer
  - Thrown Missiles: Throwing Axe, Shuriken
  - Sling 
    
See also: http://members.aol.com/dargolyt/TheForge/WPNMAIN.HTM

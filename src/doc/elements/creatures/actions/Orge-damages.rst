.. comment Included in Orge-combat.rst


Damages
-------

Damages correspond to harm to creatures or objects.

There are different types of damage, with various durations and zone of effects.

All damages have at least a type (see below) and an intensity, which is expressed in Damage Points (DP).


Damage Types
............


The type of damage steps in when the damage is inflicted to a target, whose resistance regarding this damage type is to be taken into account, as a modifier.

For example, if a Fire damage whose intensity is 110 DP is inflicted to a creature whose Fire Resistance is 15% (possibly thanks to an adequate armor), only ``110 * (1-0.15) = 94`` effective hit points will be lost by the creature.


.. Note :: Effective damages are always rounded up.



+-----------------------------+---------------------------------------------------------+--------------+
| Name of the                 | Damage Description                                      | Damage Type  |
| Damage Type                 |                                                         | Abbreviation |
+=============================+=========================================================+==============+
| Slashing (Blade)            | Physical damage caused by a sharp cutting edge. Ex: to  | SLS          |
|                             | chop a slice of dragon meat with the blade of a sharp   |              |
|                             | knife.                                                  |              |
+-----------------------------+---------------------------------------------------------+--------------+
| Piercing (Perforation)      | Physical damage caused by a pointed instrument, when    | PRC          |
|                             | perforating. Ex: a spike penetrating a chainmail.       |              |
+-----------------------------+---------------------------------------------------------+--------------+
| Bludgeoning (Impact)        | Physical damage caused by a thick and heavy instrument, | BDG          |
|                             | when crushing. Ex: a club knocking down a Halfling.     |              |
+-----------------------------+---------------------------------------------------------+--------------+
| Cold                        | Physical damage caused by very low temperature.         | CLD          |
|                             | Ex: the Icy Wimp generating a Deep Frozing Area.        |              |
+-----------------------------+---------------------------------------------------------+--------------+
| Fire                        | Physical damage caused by very high temperature (heat)  | FIR          |
|                             | and flames. Ex: the breath of the Steppe Dragon         |              |
|                             | roasted the Knight in his Plate Armor.                  |              |
+-----------------------------+---------------------------------------------------------+--------------+
| Electrical & Lightning      | Physical damage caused by electric shocks.              | ELC          |
|                             | Ex: the Shaman has been critically electrocuted by the  |              |
|                             | Altar with the Lightning Rune.                          |              |
+-----------------------------+---------------------------------------------------------+--------------+
| Arcane                      | Physical and/or Mental damage caused by supernatural    | ARC          |
|                             | forces. Ex: the Sorceress cast a Psychic Scream and     |              |
|                             | killed the Goblin Assassin just before he stroke.       |              |
+-----------------------------+---------------------------------------------------------+--------------+
| Poison                      | Physical damage caused by any agent which, when         | PSN          |
|                             | introduced into a creature organism, can produce        |              |
|                             | a noxious or deadly effect upon it.                     |              |
+-----------------------------+---------------------------------------------------------+--------------+
| Acid                        | Physical damage caused by corrosive substance attacking | ACD          |
|                             | the skin and the flesh, often very hard to remove.      |              |
+-----------------------------+---------------------------------------------------------+--------------+
| Sonic                       | Physical and Mental damage caused by very violent       | SNC          |
|                             | and disruptive sound vibrations.                        |              |
|                             | Ex: no one could tell whether he was killed first by the|              |
|                             | Roar of the HellTiger or by the Bardish Melody.         |              |
+-----------------------------+---------------------------------------------------------+--------------+





Damage Durations
................

Regarding time, damages can be:

 - punctual, like a knife stab
 - repeated (damage over time), like a poison

Unless specified otherwise, damages are punctual.


Repeated damages are defined by:

 - the number N of damage surges inflicted (ex: 5)
 - the duration D between two surges (ex: 2 seconds)

So a punctual damage of type Cold, intensity 3, will be noted as ``Cold: 3DP``, whereas a repeated damage of type Acid, intensity 15 DP, surge count 5, surge inter-duration 2 seconds, will be noted as ``Acid: 15 DP-N5-D2``.



Damage Zone of Effects
......................


To each damage inflicted, a center point, describing its actual location, is associated.

Regarding space, damages can be:

 - local, i.e. affecting one target, at the damage center point. Example: a knife stab
 - zonal, area-based, i.e. potentially affecting multiple targets, with a zone of effects described by a length L, in meters. Unless specified otherwise, this zone is a sphere whose center is the damage center point and whose radius is the associated length. Example: a *Rain of Knives* spell might affect all creatures in a 2-meter area around the target


Unless specified otherwise, damages are local.

So a local damage of type Slashing, intensity 17, will be noted as ``Slashing: 17DP``, whereas a zonal damage of type Sonic, intensity 10 DP, length (radius) 2.2 meters, will be noted as ``Sonic: 10 DP-L2.2``.


Finally, a repeated zonal damage of type Fire, intensity 12, surge count 50, surge inter-duration 0.1 second, length (radius) 0.2 meters, will be noted as ``Fire: 12 DP-N50-D0.1-L0.2``: only a Troll could have decided to sit in a barbecue.

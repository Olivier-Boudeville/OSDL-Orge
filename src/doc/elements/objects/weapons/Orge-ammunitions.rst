
Ammunitions
...........


Ranged weapons use generally ammunitions, which are stored in a magazines (ex: cartridges).

Thrown missiles (ex: shuriken, javelin, etc.) are not ammunitions, but weapons.




Ammunition Repository
_____________________


The Orge built-in ammunitions are represented in following tree:

..  ddd:raw-html:`<img src="ammunition-tree-negated.png"></img>`
..  ggg:raw-latex:`\includegraphics[scale=0.75]{ammunition-tree.png}`


Following attributes are used:

  - Associated Weapons: list of the weapons that can use that kind of ammunition
  - Inflicted Damages: list of all the damage types and intensities (in Damage Points) inflicted (see `Damage Types`_)
  - Weight: the weight of the ammunition itself (no magazine not taken into account here)
  - Base Value: base value, in credits (no magazine not taken into account here)


Arrow
*****

Hunt Arrow
::::::::::

+----------------------+------------------------------------+
| Hunt Arrow           |                                    |
| Characteristics      |                                    |
+======================+====================================+
| Associated           | Bows                               |
| Weapons              |                                    |
+----------------------+------------------------------------+
| Inflicted Damages    | Piercing: 10 DP                    |
+----------------------+------------------------------------+
| Weight               | 0.05 kg                            |
+----------------------+------------------------------------+
| Base Value           | 3 credits                          |
+----------------------+------------------------------------+


War Arrow
:::::::::

+----------------------+------------------------------------+
| War Arrow            |                                    |
| Characteristics      |                                    |
+======================+====================================+
| Associated           | Bows                               |
| Weapons              |                                    |
+----------------------+------------------------------------+
| Inflicted Damages    | Piercing: 20 DP                    |
+----------------------+------------------------------------+
| Weight               | 0.08 kg                            |
+----------------------+------------------------------------+
| Base Value           | 10 credits                         |
+----------------------+------------------------------------+


Bolt
****


Regular Bolt
::::::::::::

+----------------------+------------------------------------+
| Regular Bolt         |                                    |
| Characteristics      |                                    |
+======================+====================================+
| Associated           | Crossbows                          |
| Weapons              |                                    |
+----------------------+------------------------------------+
| Inflicted Damages    | Piercing: 25 DP, Bludgeoning: 8 DP |
+----------------------+------------------------------------+
| Weight               | 0.1 kg                             |
+----------------------+------------------------------------+
| Base Value           | 1 credit                           |
+----------------------+------------------------------------+


Dart
****

Regular Dart
::::::::::::

+----------------------+------------------------------------+
| Regular Dart         |                                    |
| Characteristics      |                                    |
+======================+====================================+
| Associated           | Blow-pipes                         |
| Weapons              |                                    |
+----------------------+------------------------------------+
| Inflicted Damages    | Piercing: 5 DP                     |
+----------------------+------------------------------------+
| Weight               | 0.1 kg                             |
+----------------------+------------------------------------+
| Base Value           | 1 credit                           |
+----------------------+------------------------------------+


Regular Poisoned Dart
:::::::::::::::::::::

+----------------------+------------------------------------+
| Regular Poisoned Dart|                                    |
| Characteristics      |                                    |
+======================+====================================+
| Associated           | Blow-pipes                         |
| Weapons              |                                    |
+----------------------+------------------------------------+
| Inflicted Damages    | Piercing: 5 DP, Poison: 8 DP-N20-D2|
+----------------------+------------------------------------+
| Weight               | 0.1 kg                             |
+----------------------+------------------------------------+
| Base Value           | 12 credit                          |
+----------------------+------------------------------------+




Small Stones
************


Random Small Stones
:::::::::::::::::::

+----------------------+------------------------------------+
| Small Stones         |                                    |
| Characteristics      |                                    |
+======================+====================================+
| Associated           | Crossbows                          |
| Weapons              |                                    |
+----------------------+------------------------------------+
| Inflicted Damages    | Bludgeoning: 8 DP                  |
+----------------------+------------------------------------+
| Weight               | 0.1 kg                             |
+----------------------+------------------------------------+
| Base Value           | 0 credit                           |
+----------------------+------------------------------------+




Magazines
.........

Ammunitions are stored in magazines. For example, arrows are stored in a quiver.

Magazines can be reusable (ex: quiver) or not (ex: bullet cartridge).



A character having a weapon able to use a kind of ammunition is supposed having the relevant magazine, whose bulkiness and weight are taken into account at the weapon level.

Some ammunitions (ex: bullets) come with their own magazine. In that case their bulkiness and weight are taken into account at the ammunition level.


Magazine attributes are:

  - Associated Ammunitions: list of the ammunitions that can use that kind of magazine
  - Capacity: the maximum number of ammunitions the magazine can contain
  - Switch Duration: duration needed, in seconds, to replace one magazine by another of the same kind (ex: replacing an exhausted cartridge by a new one)
  - Weight: the weight of the magazine when empty
  - Bulkiness: the bulkiness factor induced by the magazine
  - Base Value: base value, in credits
  

Magazine Repository
___________________


Quiver
******

A magazines for all sorts of arrows.


Hunt Quiver
:::::::::::


+----------------+------------+
| Hunt Quiver    |            |
| Characteristics|            |
+================+============+
| Associated     | Arrows     |
| Ammunitions    |            |
+----------------+------------+
| Capacity       | 10         |
+----------------+------------+
| Switch Duration| 1.2 second |
+----------------+------------+
| Weight (empty) | 0.2 kg     |
+----------------+------------+
| Bulkiness      | +3%        |
+----------------+------------+
| Base Value     | 10 credits |
+----------------+------------+


War Quiver
::::::::::


+----------------+------------+
| War Quiver     |            |
| Characteristics|            |
+================+============+
| Associated     | Arrows     |
| Ammunitions    |            |
+----------------+------------+
| Capacity       | 20         |
+----------------+------------+
| Switch Duration| 2 second   |
+----------------+------------+
| Weight (empty) | 0.6 kg     |
+----------------+------------+
| Bulkiness      | +5%        |
+----------------+------------+
| Base Value     | 50 credits |
+----------------+------------+


Sling Pouch
***********



Gnome Sling Pouch
:::::::::::::::::


Specifically designed for young Gnomes, allows a tremendous rate of fire.

+----------------+------------+
| Gnome          |            |
| Sling Pouch    |            |
| Characteristics|            |
+================+============+
| Associated     | Small      |
| Ammunitions    | Stones     |
+----------------+------------+
| Capacity       | 15         |
+----------------+------------+
| Switch Duration| 0.4 second |
+----------------+------------+
| Weight (empty) | 0.1 kg     |
+----------------+------------+
| Bulkiness      | +1%        |
+----------------+------------+
| Base Value     | 1 credits  |
+----------------+------------+



Large Sling Pouch
:::::::::::::::::


For all human-sized species. Black model for increased stealth.


+----------------+------------+
| Larger         |            |
| Sling Pouch    |            |
| Characteristics|            |
+================+============+
| Associated     | Small      |
| Ammunitions    | Stones     |
+----------------+------------+
| Capacity       | 35         |
+----------------+------------+
| Switch Duration| 1 second   |
+----------------+------------+
| Weight (empty) | 0.1 kg     |
+----------------+------------+
| Bulkiness      | +2%        |
+----------------+------------+
| Base Value     | 3 credits  |
+----------------+------------+



Movement
========

Movements of creatures are free and dynamically evaluated: there are no precomputed paths.

Creatures will be constrained not to wander outside the limits of the overall game world thanks to impassable obstacles completely surrounding the allowed area (ex: water, mountain, etc.). 

Le joueur sera empêché de sortir de cette zone par des obstacles intégrés dans le monde du jeu, notamment physiques (ex: mer, montagne, etc.)

Movements of a given type (ex: walking) are conditioned by the terrain being traversable for that movement type.

If a terrain is traversable, and if scenery elements (ex: furnitures, creature, etc.) allow it, and if creature is able to move (ex: not petrified), then movement is possible.

In this case the speed of a creature for a movement type is determined by:

 - the environment, including the terrain (ex: Road Terrain)
 - the species of the moving creature
 - the characteristics of the creature, including its Movement Ability
 - the state of the creature, including its fatigue, itself modulated notably by the carried weight 

After a movement, the fields of perception of the creature are updated.
  

Types of Movement
-----------------

The supported Movement Types are:
 
 - Ground movements: Walking and Running (no Jogging)
 - Water movements: Swimming
 - Air movements: Flying

Some creature have different `gaits <http://en.wikipedia.org/wiki/Gait>`_. 
For example, horses have for Ground movements: Walk, Trotting, Cantering, Galloping.

 
Movement Rate
-------------

It allows to determine at which speed a creature can move, once it has been established it can move.

It depends mostly on the type of movement and on the creature species and characteristics.

Based on the orders of magnitude (see Speed_), we have:

+-----------+-----------+----------+-----------+-----------+
| Species   | Walking   | Running  | Swimming  | Flying    |
| Movement  |           |          |           |           |
| Base Speed|           |          |           |           |
+===========+===========+==========+===========+===========+
| Human     | 1.2 m/s   | 6.5 m/s  | 1.0 m/s   | 0 m/s     |
+-----------+-----------+----------+-----------+-----------+
| Elf       | 1.3 m/s   | 6.8 m/s  | 0.6 m/s   | 0 m/s     |
+-----------+-----------+----------+-----------+-----------+
| Dwarf     | 0.5 m/s   | 2.1 m/s  | 0.8 m/s   | 0 m/s     |
+-----------+-----------+----------+-----------+-----------+
| Halfling  | 1.0 m/s   | 5.9 m/s  | 0.9 m/s   | 0 m/s     |
+-----------+-----------+----------+-----------+-----------+
| Gnome     | 0.5 m/s   | 2.3 m/s  | N/A       | 0 m/s     |
+-----------+-----------+----------+-----------+-----------+
| Goblin    | 1.1 m/s   | 5.2 m/s  | 0.9 m/s   | 0 m/s     |
+-----------+-----------+----------+-----------+-----------+
| Orc       | 1.2 m/s   | 6.4 m/s  | 0.5 m/s   | 0 m/s     |
+-----------+-----------+----------+-----------+-----------+


A few notes:

 - none of the Orge built-in species can fly
 - Humans are the best at swimming
 - Dwarves are as expected quite slow
 - Gnomes have fast pace but small strides, and most if not all will never swim
 - Orcs do not like water



Terrain Modifiers
.................


The terrain, which corresponds to a small in-world patch (less than a square meter), has the following impacts:

 - depending on the movement type, it can be traversed or not (for example an ocean would stop all creatures expect swimming and/or flying units); sometimes it depends on the actual situation (ex: a bat can fly in a bathing-room, but not a dragon)
 - it may or may not block visibility (ex: mist)
 - it may or may not block ranged weapons (ex: wall)
 - it may lead to another area (open door, stairs, hole in the ground, portal, warp, etc.), i.e. be a potential Area Gateway


Moreover, on a per-species basis, it:

 - offers a defensive bonus
 - affects the creature speed, for the supported movement types
 
Terrain modifiers should not be confused with the more global `Environment Modifiers`_, which are associated to full area, knowing that both modifiers apply when evaluating a movement.
 
 
 
Urban-related Terrains
______________________
   

Humans, Dwarves, Halflings, Goblins and Orcs are quite adapted to artificial environments.
Elves and Gnomes are less comfortable with these terrains.
  
   
   
In Building Terrain
*******************

Urban species can defend well and move fast here.

+------------------------+-----------+
| Allows Ground movement | yes       |
+------------------------+-----------+
| Allows Water movement  | no        |
+------------------------+-----------+
| Allows Air movement    | yes       |  
+------------------------+-----------+
| Blocks Visibility      | no        |
+------------------------+-----------+
| Blocks Ranged Weapons  | no        |
+------------------------+-----------+
| Area Gateway           | no        |
+------------------------+-----------+


+-----------+-----------+-----------+----------+-----------+-----------+
| Species   | Defense   | Walking   | Running  | Swimming  | Flying    |
| Modifiers | Modifier  | Modifier  | Modifier | Modifier  | Modifier  |
+===========+===========+===========+==========+===========+===========+
| Human     | +15%      | +12%      | +8%      | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Elf       | -10%      | -10%      | -15%     | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Dwarf     | +12%      | +10%      | +8%      | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Halfling  | +10%      | +10%      | +8%      | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Gnome     | -10%      | -10%      | -10%     | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Goblin    | +12%      | +8%       | +8%      | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Orc       | +5%       | +5%       | +5%      | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+



Road Terrain
************

This is quite poor for defense, but generally the most suitable terrain for ground movement.

+------------------------+-----------+
| Allows Ground movement | yes       |
+------------------------+-----------+
| Allows Water movement  | no        |
+------------------------+-----------+
| Allows Air movement    | yes       |  
+------------------------+-----------+
| Blocks Visibility      | no        |
+------------------------+-----------+
| Blocks Ranged Weapons  | no        |
+------------------------+-----------+
| Area Gateway           | no        |
+------------------------+-----------+


+-----------+-----------+-----------+----------+-----------+-----------+
| Species   | Defense   | Walking   | Running  | Swimming  | Flying    |
| Modifiers | Modifier  | Modifier  | Modifier | Modifier  | Modifier  |
+===========+===========+===========+==========+===========+===========+
| Human     | -5%       | +15%      | +20%     | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Elf       | -15%      | +0%       | +5%      | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Dwarf     | -10%      | +12%      | +15%     | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Halfling  | -5%       | +15%      | +8%      | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Gnome     | -5%       | +5%       | +10%     | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Goblin    | -5%       | +12%      | +12%     | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Orc       | -8%       | +10%      | +10%     | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+




Fortification Terrain
*********************

This corresponds typically to ramparts, tower tops, etc.

Good for defense, intermediate for movement.

+------------------------+-----------+
| Allows Ground movement | yes       |
+------------------------+-----------+
| Allows Water movement  | no        |
+------------------------+-----------+
| Allows Air movement    | yes       |  
+------------------------+-----------+
| Blocks Visibility      | yes       |
+------------------------+-----------+
| Blocks Ranged Weapons  | yes       |
+------------------------+-----------+
| Area Gateway           | no        |
+------------------------+-----------+


+-----------+-----------+-----------+----------+-----------+-----------+
| Species   | Defense   | Walking   | Running  | Swimming  | Flying    |
| Modifiers | Modifier  | Modifier  | Modifier | Modifier  | Modifier  |
+===========+===========+===========+==========+===========+===========+
| Human     | +18%      | +15%      | +20%     | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Elf       | +15%      | +0%       | +5%      | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Dwarf     | +22%      | +12%      | +15%     | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Halfling  | +20%      | +15%      | +8%      | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Gnome     | +18%      | +5%       | +10%     | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Goblin    | +18%      | +12%      | +12%     | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+
| Orc       | +15%      | +10%      | +10%     | N/A       | N/A       |
+-----------+-----------+-----------+----------+-----------+-----------+





Underground Terrains
____________________


Cave Terrain
************

Lava Terrain
************

Chasm Terrain
*************



Water-related Terrains
______________________

Swamp Terrain
*************

Shallow Water
*************

Deep Water
**********

Ice Terrain
***********

Snow-covered Terrain
********************

Bridge Terrain
**************



Plain Terrains
______________


Sand Terrain
************

Desert Terrain
**************

Steppe Terrain
**************

Savanna Terrain
***************

Grassland Terrain
*****************

Sparse Forest Terrain
*********************

Deep Forest Terrain
*******************



Elevated Terrain
________________


Hills Terrain
*************

Mountain Terrain
****************


Indoor Wall Terrain
*******************

Indoor walls blocks everything (creatures, sight, missiles) but may reveal hidden passages.

+------------------------+-----------+
| Allows Ground movement | no        |
+------------------------+-----------+
| Allows Water movement  | no        |
+------------------------+-----------+
| Allows Air movement    | no        |  
+------------------------+-----------+
| Blocks Visibility      | yes       |
+------------------------+-----------+
| Blocks Ranged Weapons  | yes       |
+------------------------+-----------+
| Area Gateway           | yes       |
+------------------------+-----------+




Mount Riding
............

As expected, a mounted character will be moving according to the movement of its mount, regardless of the character's characteristics except its total weight.




Movement Ability
................

This modifier impacts the movement speed depending on the characteristics of this specific creature.

It consists on the ``Base Movement Ability Modifier`` (BMAM), which is equal to ``Agility * 2 + Strength + Quickness - 4*NeutralPrimaryAttributeValue``, added to any skill-related modifier (ex: `Athletics`_).



Cartography
-----------

Unless specific measures are taken (use of map, compass, magical item, mapping spell, etc.), nothing special assists the player in finding directions except other in-world elements, such as helpful creatures and signposts.

As there are basically no auto-mapper, the player has to rely on its sense of direction and/or its own mapping onto graph paper.

Should an auto-mapper be provided, it would have to include a "fog of war" feature.
 


Scenery Elements
----------------

They form a second layer on top of the terrain layer, to determine the traversable locations.

The state of a scenery element includes these attributes:
  
  - Allows Ground movement: yes/no
  - Allows Water movement: yes/no
  - Allows Air movement: yes/no
  - Blocks Visibility: yes/no
  - Blocks Ranged Weapons: yes/no
  - Area Gateway: yes/no
  - Defense Modifier: positive or negative modifier; hiding behind a cupboard may improve your defense, whereas standing on a table may transform you into an easy target (in both cases the element must have allowed your movement first)
  - Movable: either ``fixed`` or a weight, in kilograms (movement does not include rotations, for doors for example)
  - Hit Points: either ``indestructible`` or a number of hit points, knowing scenery elements have no defense bonus. See also: `Destructible Objects`_

The main difference with terrains is that scenery elements have a mutable state.
Thus their attributes can change in various conditions, for example a locked door once destroyed may become an area gateway and does not block anything.

A lot of scenery elements cannot be interacted with (ex: carpets), so they are passive scenery elements.

Orge built-in active scenery elements are listed below, together with their default values.


Ground
......

This includes different kinds of indoor and outdoor grounds (ex: wooder floor, pavement, dirt, carpet, etc.) and buildings (terrace, bridge, etc.).


Walls
.....

+------------------------+----------------+
| Allows Ground movement | no             |
+------------------------+----------------+
| Allows Water movement  | no             |
+------------------------+----------------+
| Allows Air movement    | no             |  
+------------------------+----------------+
| Blocks Visibility      | yes            |
+------------------------+----------------+
| Blocks Ranged Weapons  | yes            |
+------------------------+----------------+
| Area Gateway           | no             |
+------------------------+----------------+
| Defense Modifier       | N/A            |
+------------------------+----------------+
| Movable                | fixed          |
+------------------------+----------------+
| Hit Points             | indestructible |
+------------------------+----------------+

Some other walls can be flown over, fired over, moved (ex: sliding walls for hidden passages), destroyed.


Doors
.....

+------------------------+----------------+
| Allows Ground movement | no             |
+------------------------+----------------+
| Allows Water movement  | no             |
+------------------------+----------------+
| Allows Air movement    | no             |  
+------------------------+----------------+
| Blocks Visibility      | yes            |
+------------------------+----------------+
| Blocks Ranged Weapons  | yes            |
+------------------------+----------------+
| Area Gateway           | no             |
+------------------------+----------------+
| Defense Modifier       | N/A            |
+------------------------+----------------+
| Movable                | fixed          |
+------------------------+----------------+
| Hit Points             | indestructible |
+------------------------+----------------+

Some other door can be seen through (ex: portcullis) or destroyed.

A door can be hidden, destructible, locked (with a key), based on a mechanism (driven by a lever, a button, a paving stone, etc.).


Furnitures
..........

+------------------------+----------------+
| Allows Ground movement | no             |
+------------------------+----------------+
| Allows Water movement  | no             |
+------------------------+----------------+
| Allows Air movement    | no             |  
+------------------------+----------------+
| Blocks Visibility      | no             |
+------------------------+----------------+
| Blocks Ranged Weapons  | no             |
+------------------------+----------------+
| Area Gateway           | no             |
+------------------------+----------------+
| Defense Modifier       | 0%             |
+------------------------+----------------+
| Movable                | fixed          |
+------------------------+----------------+
| Hit Points             | indestructible |
+------------------------+----------------+

They are almost passive objects, they often just get in the way of creatures.

Furnitures like cupboards and chests are also `Containers`_.



Stairs
......

+------------------------+----------------+
| Allows Ground movement | yes            |
+------------------------+----------------+
| Allows Water movement  | no             |
+------------------------+----------------+
| Allows Air movement    | yes            |  
+------------------------+----------------+
| Blocks Visibility      | yes            |
+------------------------+----------------+
| Blocks Ranged Weapons  | yes            |
+------------------------+----------------+
| Area Gateway           | yes            |
+------------------------+----------------+
| Movable                | fixed          |
+------------------------+----------------+
| Hit Points             | indestructible |
+------------------------+----------------+

Stairs are like doors, but they block sight and missiles, and are seldom mutable.


Portals
.......

+------------------------+----------------+
| Allows Ground movement | yes            |
+------------------------+----------------+
| Allows Water movement  | yes            |
+------------------------+----------------+
| Allows Air movement    | yes            |  
+------------------------+----------------+
| Blocks Visibility      | yes            |
+------------------------+----------------+
| Blocks Ranged Weapons  | yes            |
+------------------------+----------------+
| Area Gateway           | yes            |
+------------------------+----------------+
| Movable                | fixed          |
+------------------------+----------------+
| Hit Points             | indestructible |
+------------------------+----------------+

Portals can be created by magic, and thus may exist dynamically in non-predetermined places.

Pits, shafts, wells can be seen as portals which may kill creatures falling in them, or make them arrive into new places, etc.). 


Windows
.......

They block movement but not visibility.

They include:

 - classical windows
 - loopholes
 
 

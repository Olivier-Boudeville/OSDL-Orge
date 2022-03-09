Creature Behaviours
-------------------



Pets
----

Some species tend to be tamable by correctly trained characters (see the Taming_ skill), notably:

 - rats
 - dogs
 - wolves
 - horses
 - dragons


Non-Player Characters (NPC)
---------------------------

NPC have generally their own agenda: by default, they will live their lives, regardless of players.


Creature Relationships
----------------------


Affinities
..........


There are latent friendship or animosity between species. Affinity corresponds mostly to trust and empathy that are felt *a priori*, based on cultural learnings.

To evaluate to what extent a creature of species X likes a priori a creature of species Y, select X in the table columns ("point of view of X"), and find the row corresponding to Y ("towards Y"):


+---------------+--------------+--------------+--------------+--------------+--------------+--------------+--------------+
| Inter-species | Human        | Elf          | Dwarf        | Halfling     | Gnome        | Goblin       | Orc          |
| Affinity      | Point of View| Point of View| Point of View| Point of View| Point of View| Point of View| Point of View|
| Modifiers     |              |              |              |              |              |              |              |
+===============+==============+==============+==============+==============+==============+==============+==============+
| Towards       | -5%          | -5%          |  -5%         | -5%          | -10%         | -30%         | -45%         |
| Humans        |              |              |              |              |              |              |              |
+---------------+--------------+--------------+--------------+--------------+--------------+--------------+--------------+
| Towards       | +5%          | +15%         | -30%         | +15%         | +10%         | -40%         | -80%         |
| Elves         |              |              |              |              |              |              |              |
+---------------+--------------+--------------+--------------+--------------+--------------+--------------+--------------+
| Towards       | -5%          | -30%         | +25%         | +5%          | +0%          | -50%         | -60%         |
| Dwarves       |              |              |              |              |              |              |              |
+---------------+--------------+--------------+--------------+--------------+--------------+--------------+--------------+
| Towards       | +8%          | +0%          | +5%          | +20%         | +5%          | -25%         | -30%         |
| Halflings     |              |              |              |              |              |              |              |
+---------------+--------------+--------------+--------------+--------------+--------------+--------------+--------------+
| Towards       | -8%          | +10%         | +0%          | +5%          | +20%         | -10%         | -15%         |
| Gnomes        |              |              |              |              |              |              |              |
+---------------+--------------+--------------+--------------+--------------+--------------+--------------+--------------+
| Towards       | -35%         | -45%         | -55%         | -30%         | -20%         | +10%         | +5%          |
| Goblins       |              |              |              |              |              |              |              |
+---------------+--------------+--------------+--------------+--------------+--------------+--------------+--------------+
| Towards       | -55%         | -80%         | -75%         | -60%         | -40%         | +5%          | +15%         |
| Orcs          |              |              |              |              |              |              |              |
+---------------+--------------+--------------+--------------+--------------+--------------+--------------+--------------+


Note that affinities are not symmetrical: for example, X may like Y, whereas Y may despise X.

The point of view of a creature on its own species may be not neutral.

Animosities result in species-specific charisma modifiers during interactions, equal to this corresponding *Inter-species Affinity Modifier*.


The behaviour of a creature may be at least partly dictated by its environment (ex: hunting during the night).

See also: `Reputation`_.

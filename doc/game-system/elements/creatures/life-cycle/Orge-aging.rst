Aging
-----

Another significant factor weighing on a character's life is the age of that character. Age is managed thanks as an unsigned integer number of years, and updated once per simulated year.


Life Expectancy
...............

Should no brutal death occur, any creature will die when its life expectancy has been reached. The creature lifespan is determined at its creation (see the *Longevity* primary attribute), and is generally only known by the game system. 

Life expectancy is computed the following way. To the average lifespan corresponding to the species of the creature, modifiers are added:

 - the constitution modifier of that creature
 - a 8% bonus if it is a female creature, otherwise a 8% malus
 - a random modifier in the -10% to 10% range
 
As stated in the `orders of magnitude for time`_ section, life expectancy is medieval times was quite shorter than nowadays, approximately 40 years. Thus if one played with one's character for three years, the character actually gained 24 years, i.e. spent more than half of its life in the meantime. 



Impact of Age on Abilities
..........................

During the life expectancy of a character, various stages will be reached, affecting his abilities (actually, his primary atributes), physical as well as mental, through an age multiplicative modifier. 

For example, a creature whose base strength is 40 and whose age results in a modifier of +20% will be having currently an actual strength of ``40 + 0.2*40 = 48``.

Globally, for a human whose lifespan is 40 years (average one for a human in Orge), the age modifier will start from -90% in the first year of the character, increase steadily until a threshold of maturity is reached (at around 16 years). This will correspond to the peak abilities (modifier: +120%). They will then decrease a bit until stabilizing in a plateau (about 22 years) with a null modifier (+0%). They will remain in this good condition until about 30 years, where they will begin to decline back to zero, not unlike in a `Attack Decay Sustain Release <http://en.wikipedia.org/wiki/ADSR_envelope>`_ scheme:


This global aging profile in Orge is common to all species, genders, etc.: the same evolution will apply for all creatures, once scaled according to the effective planned lifespan of each creature. For example, if Dwarves on average live for 320 years and if a given Dwarf, Hgog, is expected to live for 355 years [#]_, then his abilities will reach zero only when being 355-year-old (not 320).

.. [#] Determined by various factors, including gender, constitution and some randomness. 
 

The impact of aging is taken into account in the form of a modifier, see `Resolving Actions`_.

Conversely, being older might imply being more experienced, trading weaken statistics against improved skills.



Textual Translation of Age
__________________________


The following *Age Table* allows to determine the age-related textual description of a creature, based on its *Age Percentage*, which is equal to its current age divided by the life expectancy of its species [#]_ :


+------------------------+---------------------------+-------------------+---------------------+
| Age Percentage Range   | Creature Age Classifier   | Corresponding     | Alternate Namings   |
|                        |                           | Human Age         |                     |
|                        |                           | For Modern Times  |                     |
+========================+===========================+===================+=====================+
| 0% - 2%                | Newborn                   | 0 - 1 years       | Toddler             |
+------------------------+---------------------------+-------------------+---------------------+
| 2% - 5%                | Child                     | 1 - 4 years       |                     |
+------------------------+---------------------------+-------------------+---------------------+
| 5% - 10%               | Boy/Girl                  | 4 - 8 years       |                     |
+------------------------+---------------------------+-------------------+---------------------+
| 10% - 17%              | Youngster                 | 8 - 14 years      |                     |
+------------------------+---------------------------+-------------------+---------------------+
| 17% - 25%              | Adolescent                | 14 - 18 years     |                     |
+------------------------+---------------------------+-------------------+---------------------+
| 25% - 40%              | Young Adult               | 18 - 30 years     | Young               |
+------------------------+---------------------------+-------------------+---------------------+
| 40% - 55%              | Adult                     | 30 - 50 years     | (no special naming) |     
+------------------------+---------------------------+-------------------+---------------------+
| 55% - 80%              | Aged Adult                | 50 - 65 years     |                     |
+------------------------+---------------------------+-------------------+---------------------+
| 80% - 100%             | Elder                     | 65 - 80 years     |                     |
+------------------------+---------------------------+-------------------+---------------------+
| 100% - 120%            | Venerable                 | 80 - 96 years     |                     |
+------------------------+---------------------------+-------------------+---------------------+
| 120% - 150% and upward | Ancient                   | 96 - 120 years    |                     |
| [#]_                   |                           |                   |                     |
+------------------------+---------------------------+-------------------+---------------------+

  
.. [#] The creature's own life expectancy is not taken into account here: not all creatures could reach the *Ancient* age classifier.

.. [#] Unless specific conditions are met during the lifespan of a creature (ex: special magic used), its life expectancy should not exceed 150% of the average one defined for its species.


For example, knowing that dwarves live on average for 320 years, and that Hgog is a 272-year-old dwarf, he could be named *Hgog, Elder Dwarf*, as his age percentage is ``272/320 = 85%``. 


Age Modifications
.................

The passage of time is at the heart of the Orge system, and no being should be able to escape it. 

However a few very uncommon ways of bypassing to some extent aging are reported to exist. These rumours should be confirmed though. They have nothing in common with necromancy, which deals with life and death, rather than with aging.

These solutions are:

 - some exceptional agreements with some deities may lead to have them promise they will grant an extended lifespan to one mortal being
 
 - the Elixir of Long Life, whose inventor and ingredients are unknown, is supposed to decrease the age of their drinker. Many Elixirs can be obtained from various potion makers, at best they have no effect once drinked
 
 - it is said that deep in the lower world, two streams flow from a small spring. Drinking from one will slow down aging, whereas drinking from the other will haste it; once a character drank from one of the two, neither will have effect on it anymore  


In Orge the safest way to enjoy a long life is to:

 #. be a creature of a species known to lead to long lifespans, like for Elves
 #. avoid to go adventuring widly, which leads too often to brutal death, like for Orcs
 #. eat five fruits and vegetables a day

 

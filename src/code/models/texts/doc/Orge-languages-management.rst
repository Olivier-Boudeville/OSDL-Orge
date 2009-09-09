
=========================
Orge Languages Management
=========================

---------------------
In-Game Words & Names
---------------------


Requirements
============

Being able to generate a word that could belong to a specified language is especially useful for:
 
 - players, so that they can be offered a set of adequate names for their characters, taking into account notably their: 
 
  - species, for example, Elves and Dwarves are not expected to have similar names
  
  - gender, as most species make use of rather differentiated names for male and female characters 

 - game designers, so that they can create plots, locations, non-playing characters, etc., which usually are in need of numerous names

A second useful feature is to let the user enter name in the Orge world while providing a way of checking whether these names have their place in the setting.

This allows to avoid Orcs being named <em>Legolas</em> or <em>Terminator</em>.



What Orge Provides
==================

According to [MC1]_, we defined here a stationary Markov chain of order 2: the future state (next letter) depends on the past two states (letters).



How It Works
============


Learning The Vocabulary of a New Language
-----------------------------------------

First, the vocabulary of the given language must be learnt. To do so, a sufficient word set must be fed to the language manager.

The word set is a simple text file containing the largest possible sample of the language, i.e. a series of words separated by whitespaces, with or without duplicates.

The language manager reads that file, and does not attempt to remove the duplicates. That way, one can increase the weight of any series of letters.

Words are then standardized (ex: put in lowercase) and then analyzed one by one. Each word is split into a series of letters, and we record, in a global tree, how many times a given letter is following another one.

The global tree is the memory

For example, if parsing the word *Video*, it becomes *video* and we record that the first letter is *v*, then that we count 

Known letters are all from ``a`` to ``z``, plus: ``-`` .

Finally, if the option ``generate_original_only``, we ensure that no generated word belongs to the word set. 



Sources of Language Wordsets
============================ 

 - `Chris Pound's language machines <http://www.ruf.rice.edu/~pound/#datasets>`_



References
==========


.. [MC1] `Markov chain <http://en.wikipedia.org/wiki/Markov_chain>`_
 
.. [MC2] `Letter-Level Markov Text <http://www.cs.bell-labs.com/cm/cs/pearls/sec153.html>`_



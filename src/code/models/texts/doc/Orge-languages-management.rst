
========================
Orge Language Management
========================

.. role:: raw-html(raw)
   :format: html
   
.. role:: raw-latex(raw)
   :format: latex



-----------------------------------
In-Game Processor For Words & Names
-----------------------------------


:Author: Olivier Boudeville
:Contact: olivier.boudeville@esperide.com
:Abstract: 

	This document explains what Orge provides in terms of language management, both for word creation and evaluation.

	Its official source and latest version is available as a `web page <http://osdl.sourceforge.net/main/documentation/OSDL/Orge/texts/Orge-languages-management.html>`_ or as a `PDF <http://osdl.sourceforge.net/main/documentation/OSDL/Orge/texts/Orge-languages-management.pdf>`_ file.




Table of Contents
=================

.. contents::   



:raw-latex:`\pagebreak`


Requirements
============

Being able to generate a word that could belong to a specified language is especially useful for:
 
 - **players** of role-playing games, so that they can be offered a set of adequate names for their characters, taking into account notably: 
 
  - their *species* / *civilizations* / *cultures*: for example, Elves and Dwarves are not expected to have similar names, same thing for Russian and Americans, or Ancient Greeks and Vikings
  
  - their *gender*: most species make use of rather differentiated names for male and female people 

  - the fact that, even in the same language, *first names* and *family names* (wherever these notions are applicable) are often rather different
  
 - **game designers**, who usually need to rely on numerous original names, so that they can create more easily plots, locations, non-playing characters, etc.

A second useful feature of an appropriate language manager is to let the users enter names of their own in the Orge world, while still providing a way of checking whether these names are compatible with the setting. This allows to avoid Orcs being named *Legolas* or *Terminator77*.


Note that we are operating here at the word level. There are interesting works, like [GEN1]_, which operate at the sentence level.

.. [GEN1] `Seventh Sanctum <http://www.seventhsanctum.com/>`_, numerous inspiring expression (word-based) generators.


:raw-latex:`\pagebreak`


What Orge Provides
==================


Word Sets
---------


Definitions
...........

Let's define a *word set* for a language ``L`` as an unsorted list of case-insensitive words belonging to ``L``:

 - separated by white spaces, all punctuation marks being ignored
 - possibly with duplicated words; in that case duplicates of a word should be as frequent as this word tends to be widespread in the language

A word set can be directly chosen as a text written in the language ``L``. 

A word set can also be a way of *defining* directly a language. 

A *language variation* of ``L`` designates a subset of ``L`` corresponding to a thematic category (ex: all place names of ``L``).



Conventions
...........


Following general language variations are defined:

+----------------+-------------------------------------+-----------------------+
| Name of the    | What this variation encompasses     | Examples for the      |
| language       |                                     | English language      |
| variation      |                                     |                       |
+================+=====================================+=======================+
|``female-names``| All female names of ``L``.          |``alison``, ``sarah``  |
+----------------+-------------------------------------+-----------------------+
|``male-names``  | All male names of ``L``.            |``john``, ``sean``     |
+----------------+-------------------------------------+-----------------------+
|``surnames``    | All family names of ``L``.          |``turner``, ``redford``|
+----------------+-------------------------------------+-----------------------+
|``placenames``  | All place names of ``L``.           |``alaska``, ``boston`` |
+----------------+-------------------------------------+-----------------------+
| ``names``      | All names used in ``L``.            |``john``, ``new-york``,|
|                |                                     |``alison``, ``jackson``|
+----------------+-------------------------------------+-----------------------+
| ``words``      | All words of ``L``,                 |``farm``, ``of``,      |
|                | regardless of their role.           |``collect``, ``john``  |
+----------------+-------------------------------------+-----------------------+


Each word set file corresponds to a specific language variation and is named according to this convention::

 ``canonical name of L``-``category of the variation``-``.txt``.

For example a word set file for the female names of the Ancient Greek language is named ``ancient-greek-female-names.txt``, and may have for content::

  Anticlea Xenoclea Meda Deianara Aegialeia Clytemnestra Periboea Hesione Leda
  Helen Xanthippe Chloe Daphne Circe Orithyia Nacippe Penthesilia Sibyl Eidyia
  Actaia Actoris Aerope Aethra Aethylla Aganippe Aglaia Alcimede Amphinome
  Arne Astynome Astyoche Autolye Callianeira Canache Chione Clytie Creusa
  Cymodece Danae Deidameia Dirce Dynamene Eriphyle Eurynome Galatea Halia
  Hiera Ianassa Iaria Leucippe Limnoraea Mante Maera Melantho Melite Metaneira
  Nacippe Nemertes Nesaea Otionia Panope Perimede Periopis Pero Pherusa
  Philomele Polymede Polymele Polyxena Prote Protogoria Scarphe Speio
  Tecmessa Thaleia Theano Thoe



:raw-latex:`\pagebreak`

List of Languages & Variations Used by Orge
...........................................

Orge provides the following list of built-in languages and variations, based on word sets, coming mostly from the repositories listed in the `Sources of Language Wordsets`_ section:

  - albanian: : words
  
  - ancient egyptian: words
  
  - ancient english: words
  
  - ancient greek: 
  
    - female names
    - male names
	
  - arabic: 
  
    - male names 
	- surnames
	
  - assyrian: words
  
  - barsoomian: words
  
  - basque:
  
    - female names
    - male names
    - words
	
  - bulgarian: words
  
  - celtic: 
  
    - female names 
	- male names
	
  - chinese: words
  - english:
  
    - female names
    - male names
    - surnames
    - words
	
  - estonian: words
  
  - german: words
  
  - giants: words
  
  - glorantha: words
  
  - gothic: words
  
  - hindi: 
  
    - female names
    - male names
	
  - indonesian: words
  - italian: words
  - japanese: 
  
    - female names
    - male names
	
  - jaqaru: words
  
  - jorune: words
  
  - kakadu: words
  
  - klingon: words
  
  - latin: words
  
  - latvian:
  
    - female names
    - male names
	
  - lovecraftian: words
  
  - malay: words
  
  - maori: words
  
  - modern greek: 
  
    - female names
	- male names
	- surnames
	
  - polish: words
  
  - russian: placenames
  
  - sindarin: words
  
  - spanish:
  
    - female names
	- male names
	- surnames
	
  - sumerian: words
  
  - swahili: words
  
  - tamil: words
  
  - thai:
  
    - female names
    - male names
	
  - tsolyani: words
  
  - turkish: words
  
  - ulwa: words
  
  - viking:
  
    - female names
	- male names



All these language word sets can be downloaded and installed automatically as a whole, thanks to our `set-up-language-wordsets.sh <http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/models/texts/src/set-up-language-wordsets.sh>`_ script.

The script creates the ``language-wordsets`` directory and fills it automatically with downloaded text files, each containing a word set for all the already listed language variations.


.. Note:: Please feel free to `send us <mailto:wordsets@esperide.com>`_
 the additional original word sets that you gathered and want to share!



:raw-latex:`\pagebreak`


Word Generator
--------------

Orge provides a *Word Generator* which, once given a word set corresponding to a language variation, is then able to generate any number of words that are likely to belong to that variation.

The user may select whether generating words already in the word set is allowed. Generally words should be kept original, i.e. generated words which happen to belong to the word set should be rejected. 

Constraints can be specified, in terms of minimal and maximal lengths for the generated words, and one may request the rejection of any generated word that happens to belong to the index, the built-in list of prohibited words. Therefore words like *shitbag* will never be returned. 

As an example, you can train the language manager with words coming for the Modern Greek language, in the context of its variation about the female names, with samples like *Artemisia*, *Aspasia*, *Atalante*, *Athena*, *Basilea*, *Vasiliki*, *Berenice* (preferably a few hundreds of them).

Once trained, the language manager can generate, with respect to any specified supported constraint (ex: minimal length), as many words as needed. Here it could be *Alippe*, *Delexanasta*, *Elenice*, *Agoria*, *Heophyllis*, *Alomena*, *Teophia*, *Eudoricea*, etc., which are inventions of the engine.




:raw-latex:`\pagebreak`


Word Evaluator
--------------

Orge provides a *Word Evaluator* too which, once given:

 - a word set corresponding to a language ``L``
 - a word ``W``
 
is able to return the probability that ``W`` belongs to ``L``.

In the context of a RPG game, this allows letting the player enter any name for his character. Given a minimal demanded probability of belonging to the corresponding language variation, names can be acknowledged or not.

For example, if a game master wants a strong world homogeneity, then he can demand that all characters bear names that have at least 80% of likeliness in their respective context. If a player then suggests *Calvin* as a name for his Elven Lady character, supposing that the Orge evaluator returns for that name a probability of only 22% of belonging to the ``elf-female-names.txt`` variation, the choice of the player will be automatically rejected by the game system.

Once applied reciprocally to the previously generated words (*Alippe* and al), it returns for each of them a likeliness of belonging to the female name variation of the Modern Greek language of at least 95%. 

On the other hand, words picked in other languages, like *Tourmente*, *Apocalypse*, *Ladder*, *Boat*, *Doppel*, *Augen* have each, according to Orge, a probability of 0% of belonging to the female name variation of the Modern Greek language (despite the greek etymology of the French word *Apocalypse*). 0% is actually an exact value (not a rounded one), as at least one sequence in each of these words never occurred in the variation word set.






:raw-latex:`\pagebreak`



How It Works
============


Conventions
-----------

The Orge language manager does not have to know *a priori* the letters of the target language, not even how many of them there are: it will discover this information at learning-time, and adapt automatically. 

A Unicode-enabled version of the language manager could probably be obtained with little effort, opening the way for the generation of words in Cyrillic, Greek, Kanji, Tibetan, etc.

As the memory footprint of the learning tree is directly correlated with the number of parsed sequences, which themselves are impacted by the number of different characters in the language, one generally tries to minimize the character set.

Usually, we choose word sets which have 28 different letters:

 - all alphabetical letters, from ``a`` to ``z`` (26 of them)
 - figures are not retained
 - capitalization is not kept, all letters are down-cased
 - the letter ``-`` (dash) is kept, as we are also interested in words like ``New-York``
 - a special letter ``eow`` is introduced, for *end of word* (automatically added and managed by the manager)
 
Therefore, for Orge, ``New-York`` is a 9-letter word (7 regular letters, the dash and the end of word): ``n,e,w,-,y,o,r,k,eow``.

Below, ``^`` will mean *and* and ``|`` will refer to the conditional probabilities: ``P(A|B) = P(A^B)/P(B)``, with ``P(B)>0``.





:raw-latex:`\pagebreak`

 
Mode of Operation
-----------------  

According to the vocabulary defined in [MC1]_, The *Orge Word Generator* is based on a stationary Markov chain whose order k is arbitrary (it can be freely specified by the user), which means that the generator evaluates the probability of all possible future states (next letter) from the memory of the past k states (letters). See also [MC2]_.
 


For good results, we generally advise to take at least ``k=2``.

.. [MC1] Definition of `Markov chain <http://en.wikipedia.org/wiki/Markov_chain>`_.

.. [MC2] `Letter-Level Markov Text <http://www.cs.bell-labs.com/cm/cs/pearls/sec153.html>`_.


We thus hereby suppose that a letter is only influenced by its immediate predecessors, up to k of them, regardless of the position of this sequence of letters in the word.

Let's name ``Lk`` the letter at position k in a word, positions starting at 1 (ex: ``L2(Orge) = r``).

As always in Orge, the language manager, defined in `class_LanguageManager.erl <http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/models/texts/src/class_LanguageManager.erl?view=markup>`_, was implemented in `Erlang <http://www.erlang.org>`_, and is best integrated within the *Orge Server Infrastructure*.




:raw-latex:`\pagebreak`

Learning The Vocabulary of a New Language
-----------------------------------------

First, the vocabulary of the given language must be learnt. To do so, a sufficient word set must be fed to the language manager.

The word set is a simple text file containing the largest possible sample of the language, i.e. a series of words separated by white-spaces, with or without duplicates.

The language manager reads that file, and does not attempt to remove the duplicates. That way, one can increase the weight of any word of the language.

Words are then standardized (ex: put in lower-case) and analysed one by one. Each word is split into a series of letters, and we record, in the global tree for that variation, how many times each eligible sequence of letters, of length n for n in [1,k] (k being the selected Markov order), is found.

We develop sub-trees only when strictly necessary: when a letter in the learning word set is never followed by another letter, no sub-tree at all is created for the latter. 

Once all these patterns have been counted, they are normalized, to pre-compute once for all everything that is needed for word generation.


For example, if parsing the word *Video*, it becomes *video* and we record that the first letter is *v*, then that increments the number of times a *v* was followed by a *i*, etc.

At the end of the learning process, we can rely on a global tree which is the memory of the language.
 
For example, if the user selected Markov chains of order 2, learning a language variation implies that for all letters X, Y, Z we can evaluate the following probabilities:
 
  - ``P( Lk+2 = Z | Lk=X ^ Lk+1=Y )``, for any k greater or equal to 1
  - ``P( L2 = Y | L1=X )``
  - ``P( L1 = X )``




:raw-latex:`\pagebreak`


Generating New Words
--------------------

Once a language variation has been learnt, we can rely on its tree, which records the probability of sequences of letters, to be already built. 

Generation of new words is then simple: for the first letter, we know readily, for all letter ``X``, ``P( L1 = X )`` so we can select, based on a uniform random law, the first letter, that we will name ``W1``.

We can then readily determine, for all letters Y, ``P( L2 = Y | L1=W1 )``, which results on the uniform drawing of ``W2``. Then ``W3`` will be obtained from ``P( Lk+2 = Z | Lk=W1 ^ Lk+1=W2 )``. Here, due to the limited memory of the Markov chain of order 2, starting from ``W4`` the very first letters will not matter any more when selecting newer letters: ``W4`` will be selected based on, for all letter Z, ``P( L4 = Z | L2=W2 ^ L3=W3 )``, and will not be directly dependant from ``W1``. 


Finally, rejection rules can be applied to the generated words, including:

 - if the option ``generate_original_only`` is set, all generated words that happen to belong to the original word set will be rejected
 
 - if the option ``{min_length,Min}`` is defined, all generated words whose length is strictly lower than ``Min`` will be rejected
 
 - if the option ``{max_length,Max}`` is defined, all generated words whose length is strictly higher than ``Max`` will be rejected
 
 - if the option ``{prohibited_index,Content}`` is defined, with ``Content`` being the path to a file listing all prohibited words, then no returned generated word will ever match any of these words. If using only ``prohibited_index`` as a single standalone option, then the Orge built-in prohibited index, based on [NoSwearing]_ (thanks for their sharing), will be used
 
 .. [NoSwearing] `Bad Word List & Swear Filter <http://www.noswearing.com/list.php>`_




:raw-latex:`\pagebreak`


Evaluating the Probability that a Word Belongs to a Variation
-------------------------------------------------------------

For the sake of this example, let's suppose that we are using Markov chains of order 2 and that:

 - we learnt a language variation ``V``, which implies that for all letters X, Y, Z we can evaluate the following probabilities:
 
  - ``P( Lk+2 = Z | Lk=X ^ Lk+1=Y )``, for any k greater or equal to 1
  - ``P( L2 = Y | L1=X )``
  - ``P( L1 = X )``
  
 - we want to evaluate the probability ``Pv`` that the word ``Orge`` belongs to ``V``, which relates to ``Pv = P( L1=o ^ L2=r ^ L3=g ^ L4=e ^ L5=eow )``.

.. Note::
	Actually ``Pv`` is not exactly the probability that *Orge* belongs to V, but the probability that a random word of V is *Orge*. The latter is clearly related to the former, but yields to far lower probabilities, that have to be corrected. For that we use a Sigmoid function. For example, if V is a two-letter (``a`` and ``b``) language with equal probability of showing up regardless of the past letters, then ``Pv(ab)=1/4`` whereas the probability that ``ab`` belongs to V is ``1``. An additional correction, based on the length, has also to be applied, as the more letters a word has, the smallest ``Pv`` gets, as we will see later, due to the increased number of multiplications with probability values, which are lower than 1.
	

For the sake of illustration, let's evaluate ``Pv`` for *Orge*, i.e.:

``Pv = P( L1=o ^ L2=r ^ L3=g ^ L4=e ^ L5=eow )``.

By definition of the `conditional probability <http://en.wikipedia.org/wiki/Conditional_probability>`_, we have ``P(A^B) = P(B|A).P(A)`` 

With ``A = L1=o ^ L2=r ^ L3=g ^ L4=e`` and ``B = L5=eow`` it leads to:

``Pv = P( L5=eow | L1=o ^ L2=r ^ L3=g ^ L4=e ). P( L1=o ^ L2=r ^ L3=g ^ L4=e )``

As we have seen, a letter depends only of its two predecessors, thus for the first term:

``P( L5=eow | L1=o ^ L2=r ^ L3=g ^ L4=e ) = P( L5=eow | L3=g ^ L4=e ) = P( Lk+2=eow | Lk=g ^ Lk+1=e)`` (which means that ``o`` and ``r`` are too distant to have an effect on ``eow``). 

We can see in the hypotheses that this probability is already available in the variation tree.

As for the second term, ``P( L1=o ^ L2=r ^ L3=g ^ L4=e )``, we can iterate as we did initially:

 ``P( L1=o ^ L2=r ^ L3=g ^ L4=e ) = P( L4=e | L1=o ^ L2=r ^ L3=g ). P( L1=o ^ L2=r ^ L3=g )``

First term ``P( L4=e | L1=o ^ L2=r ^ L3=g ) = P( L4=e | L2=r ^ L3=g ) = P( Lk+2=e | Lk=r ^ Lk+1=g )`` is already known.

Second term: ``P( L1=o ^ L2=r ^ L3=g ) = P( L3=g | L1=o ^ L2=r ). P( L1=o ^ L2=r )``, for which ``P( L3=g | L1=o ^ L2=r ) = P( Lk+2=g | Lk=o ^ Lk+1=r)`` is known.

Finally, ``P( L1=o ^ L2=r ) = P( L2=r | L1=o ). P( L1=o )``, both of which are known.

Thus we can fully compute the target probability:

``Pv =  P( Lk+2=eow | Lk=g ^ Lk+1=e). P( Lk+2=e | Lk=r ^ Lk+1=g ). P( Lk+2=g | Lk=o ^ Lk+1=r). P( L2=r | L1=o ). P( L1=o )``, which is maybe a bit clearer the other way round:

``Pv =  P( L1=o ). P( L2=r | L1=o ). P( Lk+2=g | Lk=o ^ Lk+1=r). P( Lk+2=e | Lk=r ^ Lk+1=g ). P( Lk+2=eow | Lk=g ^ Lk+1=e)``.

Said differently, the probability for a word to belong to a language variation can be deduced simply from the product of the conditional probabilities of each of its letters, knowing the previous letters, up to the order of the Markov chain.




 
:raw-latex:`\pagebreak`



Sources of Language Wordsets
============================ 

 - `Chris Pound's language machines <http://www.ruf.rice.edu/~pound/#datasets>`_
 - `Kate Monk's Onomastikon <http://www.gaminggeeks.org/Resources/KateMonk/>`_ (sadly *for personal use only*)









:raw-latex:`\pagebreak`


Resolving Actions
=================


What for?
---------

In a game, the success of all kinds of attempts of actions have to be evaluated, like when: 

 - climbing a wall
 - detecting a trap
 - resisting to a poison
 - attacking a foe
 - parrying an attack 
 - casting a spell

It is called a *check*.

Not all actions should be always evaluated: only ones that have a significant probability of failing and/or of impacting the story.

Actually the criterion should be that the events that should be specifically tested are the ones whose probability of occurence times importance of outcome is greater than a given threshold.

For example, under normal circumstances, no need to evaluate to which extent a character succeeds in breathing or walking. 

When no Game Master is available, or when too cumbersome computations are needed, the game system has to choose by itself whether each action succeeds or fails. 

Most if not all computations should be hidden to the player, and issues and outcomes should be described narratively instead.


When?
-----

The outcome of an action should be determined only when all the elements of that action are known, as a set of transactions.

For example, if a Berseker tries to behead a Rogue, the outcome of the operation will not be computed once the Berseker attacked but once the Rogue chose its reaction, for instance parrying or dodging. Then only all relevant parameters will be available.

The general rule of thumb is that an action is evaluated on the target-side rather than on the source-side. Moreover, to a single action source multiple targets can correspond (ex: zonal spell).

Finally, all the solutions to escape from an action or from its effects (analogous to saving throws) are determined based on the target-side supported reactions, i.e. the action itself does not know all the different ways of escaping it (it has however at least a default rule, should no other target-specific rule apply).


How?
----

We based our system on the one Hirinkaël described `here <../../../club/game/numericBook/ThresholdSimulationSystem.pdf>`_ (in French), most choices and remarks regarding the evaluation of actions are just taken from this document, adapted a bit and translated. 


It is done by evaluating the ``Probability Of Success (POS)`` p (say, between 0 and 100%), and drawing a random value r in, ``[0,100]``. If ``r<p``, then the action succeeds, if ``r>p`` it fails, otherwise ``r=p`` and the outcome is neither a success nor a failure (for the sake of simplicity we will consider from now on that the action is then a success).

The core of the problem is to have the game system select an accurate, realistic value for p, the probability of success for that specific action.

The intended outcome of this part of the rule system is not a predetermined "one size fits all" probability of success: we want a probability that matches closely the set of specific and hardly predictable circumstances that exist at this moment of the game (ex: light, time of day, weather, location, temperature, characters and objects involved, etc.).

As the game creator cannot precompute all the possible contexts (as they are too numerous, due to the combinatorial explosion of possible in-world variations), the game system has to rely on rules that can determine on the fly, in the light of a specific context, a global probability of success.
 

We chose to rely on `Hirinkaël's system <../../../club/game/numericBook/ThresholdSimulationSystem.pdf>`_ because:

 - it can easily take into account all kinds of situations
 - it is logical, consistent, symmetrical and fair
 - it is original
 - it is quite simple to understand and use
 - it can be used both in video games (thus being computer-based) and in table-top games (with dice and an abacus)
 
 
The reason we like this system lies mainly in the first advantage listed (flexibility, adaptability), as it offers us a way of adapting to an actual situation (real-life, complex, full of context) the chances of success of a given action which was modelled under normal circumstances (nominal conditions) only.


Two inputs: base probability & modifiers
---------------------------------------- 

Let's take, from the document we just mentioned, the example of a character wanting to attack another, and let's suppose we can evaluate the base probability, i.e. the probability of success of this action *under normal circumstances*.

The problem is that, in real cases, we are never in that stereotypical situation: the characters might be fighting in the dark, or one may be wounded, or there may be multiple opponents, etc. How to obtain, from the probability evaluated under normal circumstances, a custom probability reflecting all these numerous elements of context ?

What we really need actually is to be able to take into account probability modifiers that:

 - can be defined once for all, not depending on the base probability nor on any other modifier
 - can convert the base probability into the target context-dependent probability realistically (ex: it must be more difficult indeed to hit when wounded) and correctly (ex: probabilities must remain in the [0,1] range) 


One output: the actual context-dependent probability
---------------------------------------------------- 

To handle modifiers we retained the function suggested in the aforementionned paper:

nn nn:raw-html:`<img src="probability-modifier-formula-negated.png"></img>`
nn nn:raw-latex:`\includegraphics[scale=0.75]{probability-modifier-formula.png}`

.. comment fp(x) = p.exp(x)/(1+p.(exp(x)-1))


This function, when given a base probability (p, abscissa) and a modifier (m, select the corresponding curve), determines the resulting modified probability (pm, ordinate):

nn:raw-html:`<img src="probability-modifier-negated.png"></img>`
nn:raw-latex:`\includegraphics[scale=0.75]{probability-modifier.png}`



The wider curves, representing modifiers of -50%, 0% and 50%, allow to find easily the curves for intermediate modifiers, as they are paced every 10%.

One can see the symmetry of modifiers: the value of the increase in probability due to a given modifier p is equal to the value of the decrease in probability due to a modifier of -p.

The `probability-modifier.py script <http://osdl.svn.sourceforge.net/viewvc/osdl/OSDL/trunk/src/doc/web/main/documentation/OSDL/OSDL-engine/probability-modifier.py?view=markup>`_ gives some more indications:

::

  For a base probability of success of 0.0 %, with a modifier of:
    -30.0 %, modified probability is 0.0 %.
    -20.0 %, modified probability is 0.0 %.
    -10.0 %, modified probability is 0.0 %.
    0.0 %, modified probability is 0.0 %.
    10.0 %, modified probability is 0.0 %.
    20.0 %, modified probability is 0.0 %.
    30.0 %, modified probability is 0.0 %.

  For a base probability of success of 25.0 %, with a modifier of:
    -30.0 %, modified probability is 8.63 %.
    -20.0 %, modified probability is 12.5 %.
    -10.0 %, modified probability is 17.9 %.
    0.0 %, modified probability is 25.0 %.
    10.0 %, modified probability is 33.6 %.
    20.0 %, modified probability is 43.5 %.
    30.0 %, modified probability is 54.0 %.

  For a base probability of success of 50.0 %, with a modifier of:
    -30.0 %, modified probability is 22.0 %.
    -20.0 %, modified probability is 30.1 %.
    -10.0 %, modified probability is 39.6 %.
    0.0 %, modified probability is 50.0 %.
    10.0 %, modified probability is 60.3 %.
    20.0 %, modified probability is 69.8 %.
    30.0 %, modified probability is 77.9 %.

  For a base probability of success of 75.0 %, with a modifier of:
    -30.0 %, modified probability is 45.9 %.
    -20.0 %, modified probability is 56.4 %.
    -10.0 %, modified probability is 66.3 %.
    0.0 %, modified probability is 75.0 %.
    10.0 %, modified probability is 82.0 %.
    20.0 %, modified probability is 87.4 %.
    30.0 %, modified probability is 91.3 %.

  For a base probability of success of 100. %, with a modifier of:
    -30.0 %, modified probability is 100. %.
    -20.0 %, modified probability is 100. %.
    -10.0 %, modified probability is 100. %.
    0.0 %, modified probability is 100. %.
    10.0 %, modified probability is 100. %.
    20.0 %, modified probability is 100. %.
    30.0 %, modified probability is 100. %.

  For a base probability of success of 60.0 %, with first modifier being 20.0 %,
   with second modifier being 15.0 %, modified probability is 86.7 %.
  For a base probability of success of 60.0 %, with first modifier being 15.0 %,
   with second modifier being 20.0 %, modified probability is 86.7 %.
  For a base probability of success of 60.0 %, with a modifier of 35.0 %,
  modified probability is 86.7 %.


We can see that if the base probability is 100% or 0%, the action will be always respectively a success or a failure, not depending on the modifier. 

Moreover, multiple modifiers can be applied, applying m1 then m2 results in the same as applying m2 then m1, which is (in practice) the same as applying directly ``m3 = m1 + m2``. 


How to choose the rule inputs ?
-------------------------------

If determining the base probability is part of the usual tasks of the seasoned Game Master, modifiers, as used here, are specific to this system.

The numerical rules have been calibrated so that, when base probability is at 50%, a modifier equal to m will result in a final probability equal to about ``50+m %``. 


So, to evaluate the value of a modifier modelling a specific element of context, the Game Master just has to imagine the case where:

 - we are under normal circumstances 
 - and the action is to be performed by a candidate who has 50% of chance of succeeding, 50% of chance of failing
 
 
Then, should the studied element of context occur, what would be the new probability of success (pnew) ? Once that value has been evaluated by the Game Master, the modifier is just equal to that value minus 50%: ``m = pnew - 50%``.

More precisely, the previous numerical recipe (``50+m %``) is an approximation, mostly accurate for small values of m. The real relation is: if the Game Master chooses, for the aforementioned probability of success, pnew, then ``m = 1 / (1+ exp(-pnew))``.

It leads to this abacus, helping to evaluate this modifier:

nn:raw-html:`<img src="modifier-abacus-negated.png"></img>`
nn:raw-latex:`\includegraphics[scale=0.75]{modifier-abacus.png}`


We can see that in most situations it is perfectly safe to stick to ``m = pnew - 50%``.


Let's illustrate with a simple example
--------------------------------------


Let's suppose that a given dark ninja has a nominal probability of climbing a given wall successfully equal to 80% (ninjas are somewhat gifted for that kind of exercise). Let's suppose as well that we are at night (note that it is a bonus for our dark ninja) but it is raining (malus) and sadly he has just been wounded by an alligator (big malus).  

As modifiers are made to be context-free, we need here to evaluate only the impact of a given modifier against the nominal situation, without having to bother with any other modifier (we suppose here all modifiers are independent, although one may argue it is an oversimplification here). 

Let's take the malus due to the wound. We may consider that a wounded dark-ninja at daylight, dry weather, incures a 35% penalty when trying to climb that kind of wall.

The same exercise may result in a 10% bonus for the climbing of an unwounded ninja, dry weather, but in the night, and in a 5% malus for the climbing of an unwounded ninja, during the day, but under the rain.

So, what is the overall probability of successful climbing, for our wounded ninja under the rain at night time ? Modifiers have to be summed, it results in a global modifier of ``-35+10-5=-30``. We then look at our curves: the intersection of the ninja base probability of success (80%) with the curve of the -30% modifier gives us a final probability of about 53%. 0ur ninja finally has little more chance to arrive at the top of the wall than to fall miserably in the flowers.


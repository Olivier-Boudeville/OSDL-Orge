
:raw-latex:`\pagebreak`


Reactions
---------

Reactions (sometimes called *saving throws*) correspond to the possibilities for a target to react immediatly to an event, in general in order to escape its consequences.

Reactions are expressed thanks to target-side modifiers that are to be applied to the base probability of the associated event, in the context of that target. 

A reaction of a target might reduce or increase the likeliness that the event affects it. Therefore, sometimes it is better not to try to react. For example, a creature will a weak wisdom should better not try to resist to confusion, lest it worsen its situation.

Modifiers are expressed as signed percentages to be added to Probabilities of Success. They are generally weighted sums of character attributes, knowing that some attributes might act against the reaction. For example, Fear can be reduced a lot by a strong Willpower, but on the contrary Intelligence will tend to increase Fear.


For example, reactions can be triggered against actions (which are special cases of events), like attacks, spells, or against sudden events, like a collapsing ceiling.

Reactions to attacks are discussed in the `Defenses`_ section.

Here are listed default and generic reactions, made to be shared for similar events.

Some specific reactions supersede more general ones: for instance, in the case of a paralysis-inducing poison, *Resisting (Physical) Paralysis* should be used instead of *Resisting Poisons*. 


Reactions should not be confused with `Damage Resistance`_: they allow to avoid the consequences of an event. Should they fail, the event will affect the character, which may result in damages being dealt, and the damage resistance to step in.


.. comment Make reactions depend on skills and species as well. Describe consequences (ex: paralysis)
.. comment Add Petrification



Avoiding Traps
..............

When a trap is triggered by a target, the action incurs a modifier in its Probability Of Success (POS) which depends on the Quickness and, to a lesser extent, Dexterity of the target: ``Trap Modifier = (2*Quickness + Dexterity - 3*NeutralPrimaryAttributeValue) * 8%``.
 

Resisting Charms
................

All effects inducing a charm on their target incur a modifier in their Probability Of Success (POS) which depends on the Charisma and Willpower of the target: ``Charm Modifier = (Charisma + Willpower - 2*NeutralPrimaryAttributeValue) * 8%``.



Resisting Confusion
...................

All effects inducing confusion on their target incur a modifier in their Probability Of Success (POS) which depends on the Wisdom of the target: ``Confusion Modifier = (Wisdom - NeutralPrimaryAttributeValue) * 5%``.



Resisting Disease
.................

All effects inducing a disease on their target incur a modifier in their Probability Of Success (POS) which depends on the Constitution of the target, and marginally on its Willpower: ``Disease Modifier = (3*Constitution + Willpower - 4 * NeutralPrimaryAttributeValue) * 5%``.



Resisting Fear
..............

All effects inducing fear on their target incur a modifier in their Probability Of Success (POS) which depends on the Willpower of the target, and marginally on its Intelligence, which worsen the fear: ``Fear Modifier = (4*Willpower - Intelligence - 3* NeutralPrimaryAttributeValue) * 5%``.



Resisting Mana Drain
....................

All effects inducing a mana drain on their target incur a modifier in their Probability Of Success (POS) which depends on the Wisdom and Intelligence of the target: ``Mana Drain Modifier = (Wisdom + Intelligence - 2*NeutralPrimaryAttributeValue) * 15%``.

Mana Drain should not be confused with Mental Drain.



Resisting Mental Drain
......................

All effects inducing a mental drain (i.e. a gains of mental fatigue points) on their target incur a modifier in their Probability Of Success (POS) which depends on the Willpower of the target: ``Mental Drain Modifier = (Willpower - NeutralPrimaryAttributeValue) * 12%``.

Mental Drain should not be confused with Mana Drain.



Resisting Paralysis
...................

All effects inducing a mental paralysis (ex: spells) on their target incur a modifier in their Probability Of Success (POS) which depends on the Willpower and Intelligence of the target: ``Mental Paralysis Modifier = (Willpower + Intelligence - 2 *NeutralPrimaryAttributeValue) * 6%``.

All effects inducing a physical paralysis (ex: some specific poisons) on their target incur a modifier in their Probability Of Success (POS) which depends on the Constitution and Strength of the target: ``Physical Paralysis Modifier = (Constitution + Strength- 2*NeutralPrimaryAttributeValue) * 3%``.

Thus reactions to mental paralysis matter more than the ones to physical paralysis.



Resisting Physical Drain
........................

All effects inducing a physical drain (i.e. a gains of physical fatigue points) on their target incur a modifier in their Probability Of Success (POS) which depends on the Constitution of the target: ``Physical Drain Modifier = (Constitution - NeutralPrimaryAttributeValue) * 12%``.



Resisting Poisons
.................

All effects inducing a poisoning effect on their target incur a modifier in their Probability Of Success (POS) which depends on the Constitution of the target: ``Poison Modifier = (Constitution - NeutralPrimaryAttributeValue) * 2%``.



Resisting Other Arcane Effects
..............................

For all arcane effects not specifically described in this section, this default rule will apply.

For arcane effects from the:

 - Mage Path: ``Mage Effect Modifier = (3*Intelligence + Wisdom + Willpower - 5*NeutralPrimaryAttributeValue) * 5%``
 - Cleric Path: ``Cleric Effect Modifier = (3*Wisdom + Charisma + Willpower - 5*NeutralPrimaryAttributeValue) * 5%``
 - Bard Path: ``Bard Effect Modifier = (3*Charisma + Intelligence + Willpower - 5*NeutralPrimaryAttributeValue) * 5%``
 - unless specified otherwise, any other Path: ``Path Effect Modifier = (2*Intelligence + Wisdom + Charisma + Willpower - 5*NeutralPrimaryAttributeValue) * 5%``
 

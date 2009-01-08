
:raw-latex:`\pagebreak`

 
Orge Objects
============

Each object is determined by:

 - a name
 - a textual description
 - a size (volume), expressed in litres, notably to evaluate bulkiness
 - a base value (if applicable), expressed in credits and in world currency as well. This base value corresponds to the mean found value for that object in the game world, to be modulated by the actual merchant
 - a wear level, which determines how much the object is worn-out. This is notably useful for armors and weapons


Moreover, objects may modify the probability of success of different actions.

For example, someone having a halberd, or even knowing how to use it, will have increased chances of dismounting a cavalryman. 

 

Object Quality
--------------

 - bronze, iron, steel
 

Object Wear
-----------

The wear of an object does not correspond exactly to its hit points (see `Destructible Objects`_ instead): the former is related to its usability, the latter to its state. 

The wear of each object is described by a `Maximum Wear Level` (MWL) and a `Current Wear Level` (CWL). CWL must be in the [0;MWL] range.

The wear percentage is defined to be equal to ``CWL/MWL``.

+-----------------+--------------+--------+----------+
| Wear Percentage | Wear State   | Usable | Reparable|
+=================+==============+========+==========+
| 0%              | New          | Yes    | No       |
+-----------------+--------------+--------+----------+
| ]0%;25%[        | Lightly Used | Yes    | Yes      |
+-----------------+--------------+--------+----------+
| [25%;55%[       | Used         | Yes    | Yes      |
+-----------------+--------------+--------+----------+
| [55%;85%[       | Worn-out     | Yes    | Yes      |
+-----------------+--------------+--------+----------+
| [85%;100%[      | Broken       | No     | Yes      |
+-----------------+--------------+--------+----------+
| 100%            | Unreparable  | No     | No       |
+-----------------+--------------+--------+----------+


Each time an object is used, its wear increases by an amount which depends on the action and on its context. For example, when a dagger is used to stab a foe, the wear of the dagger is increased due to the stabbing (action) and to the resistance of the armor of the opponent (context). If it is a chain mail, the dagger may be worn sooner than if the foe had no armor.

Reciprocally, any armor would see its wear increase due to the stabbing.

When an object becomes broken, it cannot operate normally. For instance a weapon would deal little or no damage, an armor would protect a little or not at all. However these broken objects can still be used, thus they can reach an unreparable state.


Managing the wear of equipments can be an interesting gameplay element at first, but in most cases it should not be a constant concern for the player.

To alleviate when appropriate this constraint, objects with significant `Maximum Wear Level` and/or relevant repair spells and/or widespread replacements (ex: thanks to cheap tinkers) could be introduced later in the game.

Wear is managed in class_Object.erl_ and tested in class_Object_test.erl_.


Destructible Objects
--------------------

Some objects can be destroyed. They then change of nature. For example, a destroyed armchair can become a set of cloth and wood pieces.

All kinds of events can lead to the destruction of objects: blows dealt by creatures, fire, magic, etc.

In all cases, a destructible object is considered as having no defense nor special resistance, only hit points, which corresponds to the Health attribute of beings.


Movable Objects
---------------

Some objects can be moved. In this case, depending on their weight and on the ground type, a given force may or may not succeed in moving them.



.. include:: Orge-object-repository.rst


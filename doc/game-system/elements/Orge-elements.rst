.. comment Included from main document (Orge-game-system.rst)


:raw-latex:`\pagebreak`



Orge Elements
=============

Orge elements corresponds to a very general notion which encompasses objects and as well as creatures.

As they share some traits, they are described specifically here.


Resistance
----------

As described in the `Damage Types`_ section, different kinds of damage can be dealt, and some elements will resist more than others.

For example, a wooden cupboard is expected to be quite vulnerable to Fire Damage, whereas a strong-box or its living counterpart, the Knight, should be seldom afraid of a Bludgeoning Damage.

Therefore all destructible elements (all creatures can be killed, most objects can be destroyed, only parts of the settings, like walls, may or may not be altered) have their resistance against all damage types determined. See also: `Destructible Objects`_.


.. Note:: When no resistance against a damage type is specified, this implies a null (zero) resistance modifier.


Location
--------

All elements have an in-world location, corresponding to their center of gravity (origin of their local referential), and some indication of their size, either a radius or a set of lengths (front-facing length, width, height).

This location may or may not change, depending on the element being movable or not (ex: creatures generally move, wallsgenerally not).

In Orge, the world is primarily simulated in isometric 3D, with latitude and longitude defining positions on the ground. Altitude allows to determine stacking, i.e. the ascending order in which elements at the same location are placed.

All coordinates are floating-point values.

See also: `Movable Objects`_.


Areas
-----

The simulated world in Orge is partioned into a sets of unit areas. This means that each point of the game world is in one and only one unit area.

Each area is a planar 2D space, parallel to the ground, which encompasses everything that is below or above that surface. An area has itself its own referential, defined relatively to a parent area.

All positions of elements in that area are defined relatively to the referential of that area.

Non-unit area may encompass other (unit or not) areas, so that the simulated world is partitioned hierarchically. Unit areas are exactly the leaves of the corresponding tree (ex: BSP algorithms can be used to create and manage that tree).

Currently areas are static, they do not change over time (they can neither deform nor move).



Orientation
-----------

In Orge, the game world is also divided into square cells, and eight orientations are defined. They are enumerated in clockwise order, and comply with the conventions of OSDL's asset repository:

 #. North
 #. NorthEast
 #. East
 #. SouthEast
 #. South
 #. SouthWest
 #. West
 #. NorthWest



.. comment Complies with direction-identifiers.txt
.. comment in OSDL/trunk/tools/media/video/asset-repository

.. include:: Orge-creatures.rst
.. include:: Orge-objects.rst
.. include:: Orge-places.rst

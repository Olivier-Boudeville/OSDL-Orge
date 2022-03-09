====
Orge
====

--------------------
OSDL RPG Game Engine
--------------------

.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex

.. comment Orge Header


:Author: Olivier Boudeville
:Contact: olivier.boudeville@esperide.com

:Status: This is a work in progress
:Copyright: 2008-2011 Olivier Boudeville. This document, the game system it describes and its implementation are all released under a `Creative Commons <http://creativecommons.org>`_ license whose name is *Attribution-Noncommercial-Share Alike 3.0*. See `Orge License`_ for further details.

:Dedication:

	For Orge users (scenario writers, game masters, players) and system co-designers.


:Abstract:

	This document describes the **OSDL RPG Game Engine** (Orge), which is a role-playing game system, mainly dedicated to video games in an heroic-fantasy setting. It could be easily adapted as well for table-top playing and for other settings.

	**Orge** is a game system that tries to focus on generic yet lightweight guidelines and rules, by minimizing the number of concepts and associating them in a logical manner. It includes a model (a set of simulation rules) and its corresponding implementation (a program able to manage these rules).

.. meta::
   :keywords: game system, role-playing, RPG, engine, video game, OSDL, Orge
   :description lang=en: A description of the Orge game system, containing examples of all basic rules and of many advanced ones.


:raw-latex:`\pagebreak`

.. contents:: Table of Contents
	:depth: 3

.. comment .. section-numbering::



.. comment Use regularly the check-rst-includes.sh script to ensure no file is left not included or is included twice.



:raw-latex:`\pagebreak`


Overview
========

Orge, the **OSDL RPG Game Engine**, is a set of game rules to be used in *Role-Playing Games* (RPG).

Here we designate by *RPG Game Engine* the set of high-level rules that are used to simulate a virtual world.

Such a simulation can be performed either manually, i.e. with table-top games and a real Game Master, or, more importantly here, with the help of computers. In this case, an emulated Game Master manages the video game, here thanks to the Orge implementation. Both the rules and the implementation are released under this `license`_. This document concentrates mostly on the video game case.

Concerns like rendering_, `input handling`_, etc. belong to the `end-user interface`_, and are deemed here to be outside of the scope of the game engine: if the game was a networked one, we would focus here mostly on the server-side simulation of the game actors, not on the game client.

Our goal is to define in this document:

 - what are the general requirements of most of the Role-Playing Games we aim at, regarding the underlying game engine

 - what is the detailed solution we preferred to apply in order to fulfill these needs, and why

 - how to use that solution (Orge) in practise



.. comment All first-order includes should start with the title level "=".

.. comment For hyperlinks (nothing visible in document):
.. include:: Orge-implementation-references.rst

.. include:: Orge-conventions.rst
.. include:: Orge-design-decisions.rst

.. include:: Orge-elements.rst

.. include:: Orge-narrative.rst

.. include:: Orge-license.rst
.. include:: Orge-bibliography.rst
.. include:: Orge-appendices.rst

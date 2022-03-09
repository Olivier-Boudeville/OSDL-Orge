
:raw-latex:`\pagebreak`


Orge Implementation
===================


Overview
--------

The Orge game system has been implemented by the `OSDL project <http://osdl.sourceforge.net>`_. This codebase is meant to power multi-player games, notably in the context of persistent worlds, including MMORPG. 

Orge has been implemented in the `Erlang <http://www.erlang.org>`_ language. In a few worlds, Erlang has been chosen for:
 
 - its scalability and support for massive concurrency
 - its fault-tolerance abilities
 - its high-level orientation
 - the in-place incremental hot code reload it features
 
As stated in `Orge License`_, the codebase is released under a `Creative Commons <http://creativecommons.org>`_ license whose name is *Attribution Noncommercial Share Alike 3.0*. 

The full Orge codebase can be browsed from `our SVN repository <http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/>`_.

The Orge implementation is based on the services provided by sister project of OSDL, the `Ceylan project <http://ceylan.sourceforge.net>`_. Ceylan focuses on generic features, whereas OSDL concentrates on interactive, multimedia, game-related applications.


Technically
-----------

Each and every class modelling a game element should be able to send traces, thus should inherit from the ``TraceEmitter`` class.

.. include:: Orge-interface.rst

.. include:: Orge-multiplayer.rst


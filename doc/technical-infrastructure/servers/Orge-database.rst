.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


:raw-latex:`\pagebreak`

.. _class_OrgeDatabaseManager.hrl: http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/servers/functional-services/database-storage/src/class_OrgeDatabaseManager.hrl?view=markup

.. _class_OrgeDatabaseManager.erl: http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/servers/functional-services/database-storage/src/class_OrgeDatabaseManager.erl?view=markup


.. _Orge database:



Orge Database
=============

.. contents::

.. :local:


Overview
--------

The Orge database contains the various information about:

 - each Orge user
 - the simulation state
 - the monitored events, including each connection to the Orge server



Some tables of the database are expected to be in memory (RAM) only, for efficiency reasons (soft real-time needed). Regular disk snapshots are to be performed nevertheless.

Some other tables, too large and/or less frequently requested, will be stored directly on disc.

However the majority of tables will be stored both on disc (for write persistence) *and* in RAM (for fast read accesses).

The Orge database is based on Mnesia.


To inspect the data in the tables of the Orge database, one just has to run, for example from the Orge server shell, supposing Mnesia is already running (``mnesia:start().``)::

 > tv:start().

Hitting CTRL-M allows then to browse the various tables.


Conventions
-----------

Each table field has to respect a specified datatype. A datatype can define base constraints, that will be enforced at all times, between transactions.

Per-field additional constraints can be defined as well. This includes:

 - mandatory: the field has to be set (this is the default)
 - optional: the field may or may not be set


Identifiers
...........

Main information are identified by an intentionally meaningless number, to allow all related information to be changed while preserving the links between entries.

This number, the information identifier, is implemented by a simple unsigned integer counter, starting at 1, and incremented for each new entry. The related type/format is named ``OrgeIdentifier``.

The information identifier is a primary key, on which an index is created for faster look-up.

The identifiers of Orge users are set by the Orge Database, whereas the identifiers for connections are set by the Orge TCP Server.


Strings
.......

A string is a list of characters (including alphanumerical and accentued ones) with punctuation marks.

The length of a string can be limited to avoid buffer overflow. For example, ``String[37]`` means that up to 37 characters can be stored in this string, whereas ``String`` implies an unlimited length.


Time
....

It is a pair made of a date and a time, like in ``{{2008,7,11},{17,21,43}}`` for July 11, 2008, at 17:21:43.


Schema For Orge Simulation Users
--------------------------------

Orge user information are stored in a table named ``orge_user``.

An Orge user is described by the following fields:

+------------------------+-----------------------------------+-------------+--------------------------+
| Field Name             | Format/Datatype                   | Default     | Additional               |
|                        |                                   | Value       | Constraints              |
+========================+===================================+=============+==========================+
| Identifier             | OrgeIdentifier                    | N/A         | Incremented from 1.      |
+------------------------+-----------------------------------+-------------+--------------------------+
| Account Creation Time  | Time                              | None        | None                     |
+------------------------+-----------------------------------+-------------+--------------------------+
| Account Status         | In: [ ``active``, ``suspended``,  |             |                          |
|                        | ``deleted`` ].                    |             |                          |
+------------------------+-----------------------------------+-------------+--------------------------+
| Controlled Characters  | See below                         | None        | None                     |
+------------------------+-----------------------------------+-------------+--------------------------+
| First Name             | String[15]                        | None        | None                     |
+------------------------+-----------------------------------+-------------+--------------------------+
| Last Name              | String[15]                        | None        | None                     |
+------------------------+-----------------------------------+-------------+--------------------------+
| Date Of Birth          | Date                              | None        | Optional                 |
+------------------------+-----------------------------------+-------------+--------------------------+
| Address Line 1         | String[30]                        | None        | Optional                 |
+------------------------+-----------------------------------+-------------+--------------------------+
| Address Line 2         | String[30]                        | None        | Optional                 |
+------------------------+-----------------------------------+-------------+--------------------------+
| City                   | String[15]                        | None        | Optional                 |
+------------------------+-----------------------------------+-------------+--------------------------+
| State                  | String[15]                        | None        | Optional                 |
+------------------------+-----------------------------------+-------------+--------------------------+
| Country                | String[15]                        | None        | Optional                 |
+------------------------+-----------------------------------+-------------+--------------------------+
| Postal Code            | String[15]                        | None        | Optional                 |
+------------------------+-----------------------------------+-------------+--------------------------+
| Home Telephone         | String[30]                        | None        | Optional                 |
+------------------------+-----------------------------------+-------------+--------------------------+
| Mobile Telephone       | String[30]                        | None        | Optional                 |
+------------------------+-----------------------------------+-------------+--------------------------+
| Email Address          | Email                             | None        | None                     |
+------------------------+-----------------------------------+-------------+--------------------------+
| Account Login          | String[15]                        | None        | None                     |
+------------------------+-----------------------------------+-------------+--------------------------+
| Account Password       | String[15]                        | None        | None                     |
+------------------------+-----------------------------------+-------------+--------------------------+
| Security Question      | String[30]                        | None        | Optional                 |
+------------------------+-----------------------------------+-------------+--------------------------+
| Security Answer        | String[30]                        | None        | Optional                 |
+------------------------+-----------------------------------+-------------+--------------------------+

The field ``Controlled Characters`` lists the simulated characters that the Orge user can control. This field can contain only one character identifier, or a fixed-sized tuple, or a list containing character identifiers, depending on the chosen account settings for that Orge instance.

The corresponding Erlang ``record`` is defined in class_OrgeDatabaseManager.erl_.



Schema For Simulation State
---------------------------

Schema For Monitored Events
---------------------------

All connections to an Orge server are stored in a table named ``orge_connection``.

A connection is described by the following fields:

+---------------------------+-----------------------------------+-------------+--------------------------+
| Field Name                | Format/Datatype                   | Default     | Additional               |
|                           |                                   | Value       | Constraints              |
+===========================+===================================+=============+==========================+
| Identifier                | OrgeIdentifier                    | N/A         | Incremented from 1.      |
+---------------------------+-----------------------------------+-------------+--------------------------+
| Login Status              | In: [ ``not_tried_yet``,          | None        | None                     |
|                           | ``access_granted``, ``bad_login`` |             |                          |
|                           | ``bad_password``, ``timeout``,    |             |                          |
|                           | ``marshalling_failed``,           |             |                          |
|                           | ``already_connected``,            |             |                          |
|                           | ``account_not_active``].          |             |                          |
+---------------------------+-----------------------------------+-------------+--------------------------+
| User Identifier           | OrgeIdentifier                    | N/A         | Optional                 |
+---------------------------+-----------------------------------+-------------+--------------------------+
| Sent Login                | String[15]                        | None        | Optional                 |
+---------------------------+-----------------------------------+-------------+--------------------------+
| Hash of the Sent Password | String[20] (with sha)             | None        | Optional                 |
+---------------------------+-----------------------------------+-------------+--------------------------+
| Peer IP Address           | IPV4Address                       | None        | None                     |
+---------------------------+-----------------------------------+-------------+--------------------------+
| Peer TCP Port             | Port                              | None        | None                     |
+---------------------------+-----------------------------------+-------------+--------------------------+
| Connection Start Time     | Time                              | None        | None                     |
+---------------------------+-----------------------------------+-------------+--------------------------+
| Connection Stop Time      | Time                              | None        | Optional                 |
+---------------------------+-----------------------------------+-------------+--------------------------+
| Geolocated Country        | String                            | None        | None                     |
+---------------------------+-----------------------------------+-------------+--------------------------+
| Geolocated Region         | String                            | None        | None                     |
+---------------------------+-----------------------------------+-------------+--------------------------+
| Geolocated City           | String                            | None        | None                     |
+---------------------------+-----------------------------------+-------------+--------------------------+
| Geolocated Postal Code    | String                            | None        | None                     |
+---------------------------+-----------------------------------+-------------+--------------------------+

A connection is geolocated from its IP address, see `Geolocation`_ for more details.

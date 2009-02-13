

:raw-latex:`\pagebreak`


.. role:: raw-html(raw)
   :format: html
   
.. role:: raw-latex(raw)
   :format: latex


.. _orge_database_manager.hrl: http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/servers/functional-services/database-storage/src/orge_database_manager.hrl?view=markup

.. _orge_database_manager.erl: http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/servers/functional-services/database-storage/src/orge_database_manager.erl?view=markup


.. _Orge database:



Orge Database
=============

.. contents:: 
	:local:


Overview
--------

The Orge database contains the various informations about:
 
 * each Orge user
 * the simulation state
 * the monitored events, including each connection to the Orge server 


 
Most tables of the database are expected to be in memory (RAM) only, for efficiency reasons (soft real-time needed). Regular disk snapshots will be performed nevertheless.

Some tables, too large and/or less frequently requested, will be stored directly on disc.  

The Orge database is based on Mnesia.  


To inspect the data in the tables of the Orge database, one just has to launch, for example from the Orge server shell::

 > tv:start().


Conventions
-----------

Each table field has to respect a specified datatype. A datatype can define base constraints, that will be enforced at all times, between transactions.

Per-field additional constraints can be defined as well. This includes:
 
 - mandatory: the field has to be set (this is the default)
 - optional: the field may be not set


Identifiers
...........


All main informations are identified by an intentionally meaningless number, to allow all related informations to be changed while preserving the links between entries.

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


Schema For Simulation Users
---------------------------

Orge user informations are stored in a table named ``orge_user``.

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

The corresponding Erlang ``record`` is defined in orge_database_manager.erl_.
 
 
 
Schema For Simulation State
---------------------------

Schema For Monitored Events
---------------------------

All connections to an Orge server are stored in a table named ``orge_connection``.

A connection is described by the following fields:

+------------------------+-----------------------------------+-------------+--------------------------+
| Field Name             | Format/Datatype                   | Default     | Additional               |
|                        |                                   | Value       | Constraints              |
+========================+===================================+=============+==========================+
| Identifier             | OrgeIdentifier                    | N/A         | Incremented from 1.      |
+------------------------+-----------------------------------+-------------+--------------------------+
| Login Status           | In: [ ``not_tried_yet``,          | None        | None                     |
|                        | ``access_granted``, ``bad_login`` |             |                          |
|                        | ``bad_password``, ``timeout``,    |             |                          |
|                        | ``marshalling_failed``,           |             |                          |
|                        | ``already_connected``,            |             |                          |
|                        | ``account_not_active``].          |             |                          |
+------------------------+-----------------------------------+-------------+--------------------------+
| User Identifier        | OrgeIdentifier                    | N/A         | Optional                 |
+------------------------+-----------------------------------+-------------+--------------------------+
| Sent Login             | String[15]                        | None        | Optional                 |
+------------------------+-----------------------------------+-------------+--------------------------+
| Sent Password          | String[15]                        | None        | Optional                 |
+------------------------+-----------------------------------+-------------+--------------------------+
| Peer IP address        | IPV4Address                       | None        | None                     |
+------------------------+-----------------------------------+-------------+--------------------------+
| Peer TCP port          | Port                              | None        | None                     |
+------------------------+-----------------------------------+-------------+--------------------------+
| Connection Start Time  | Time                              | None        | None                     |
+------------------------+-----------------------------------+-------------+--------------------------+
| Connection Stop Time   | Time                              | None        | Optional                 |
+------------------------+-----------------------------------+-------------+--------------------------+
| Geolocated Country     | String                            | None        | None                     |
+------------------------+-----------------------------------+-------------+--------------------------+
| Geolocated Region      | String                            | None        | None                     |
+------------------------+-----------------------------------+-------------+--------------------------+
| Geolocated City        | String                            | None        | None                     |
+------------------------+-----------------------------------+-------------+--------------------------+
| Geolocated Postal Code | String                            | None        | None                     |
+------------------------+-----------------------------------+-------------+--------------------------+

A connection is geolocated from its IP address, see `Geolocation`_ for more details.


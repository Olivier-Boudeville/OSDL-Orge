

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
 
 * the Orge users (stored in the ``orge_user`` table)
 * the connections to the Orge server (stored in the ``orge_connection`` table)
 * the simulation state
 * the monitored events


 
Most tables of the database are expected to be in memory (RAM) only, for efficiency reasons (soft real-time needed). Regular disk snapshots will be performed nevertheless.

Some tables, too large and/or less frequently requested, will be stored directly on disc.  

The Orge database is based on Mnesia.  


Conventions
-----------

Each table field has to respect a specified datatype. A datatype can define base constraints, that will be enforced at all times between transactions.

Per-field additional constraints can be defined as well, this includes:
 
 * mandatory: the field has to be set (this is the default)
 * optional: the field may be not set


Identifiers
...........


All main informations are identified by an intentionally meaningless number, to allow all related informations to be changed while preserving the links between entries.

This number, the information identifier, is implemented by a simple unsigned integer counter, starting at 1, and incremented for each new entry. The related type/format is named ``OrgeIdentifier``.

The information identifier is a primary key, on which an index is created for faster look-up.

The identifiers for Orge users are set by the Orge database, whereas the identifiers for connections are set by the Orge TCP server.


Strings
.......

A list of characters (including alphanumerical and accentued ones) with punctuation marks.

The length of a string can be limited to avoid buffer overflow. For example, ``String[37]`` means that up to 37 characters can be stored in this string, whereas ``String`` implies an unlimited length.



Schema For Simulation Users
---------------------------

Orge users are stored in a table named ``Users_Table``.

An Orge user is described by the following fields:

+------------------------+-------------------+----------+--------------------+
| Field Name             | Format/Datatype   | Default  | Additional         |
|                        |                   | Value    | Constraints        |
+========================+===================+==========+====================+
| Identifier             | OrgeIdentifier    | N/A      | None               |
+------------------------+-------------------+----------+--------------------+
| First Name             | String[15]        | None     | None               |
+------------------------+-------------------+----------+--------------------+
| Last Name              | String[15]        | None     | None               |
+------------------------+-------------------+----------+--------------------+
| Date Of Birth          | Date              | None     | Optional           |
+------------------------+-------------------+----------+--------------------+
| Address Line 1         | String[30]        | None     | Optional           |
+------------------------+-------------------+----------+--------------------+
| Address Line 2         | String[30]        | None     | Optional           |
+------------------------+-------------------+----------+--------------------+
| City                   | String[15]        | None     | Optional           |
+------------------------+-------------------+----------+--------------------+
| State                  | String[15]        | None     | Optional           |
+------------------------+-------------------+----------+--------------------+
| Country                | String[15]        | None     | Optional           |
+------------------------+-------------------+----------+--------------------+
| Postal Code            | String[15]        | None     | Optional           |
+------------------------+-------------------+----------+--------------------+
| Home Telephone         | String[30]        | None     | Optional           |
+------------------------+-------------------+----------+--------------------+
| Mobile Telephone       | String[30]        | None     | Optional           |
+------------------------+-------------------+----------+--------------------+
| Email Address          | Email             | None     | None               |
+------------------------+-------------------+----------+--------------------+
| Account Login          | String[15]        | None     | None               |
+------------------------+-------------------+----------+--------------------+
| Account Password       | String[15]        | None     | None               |
+------------------------+-------------------+----------+--------------------+
| Security Question      | String[30]        | None     | Optional           |
+------------------------+-------------------+----------+--------------------+
| Security Answer        | String[30]        | None     | Optional           |
+------------------------+-------------------+----------+--------------------+
| Account Creation Date  | Date              | None     | Optional           |
+------------------------+-------------------+----------+--------------------+
| Controlled Characters  | See below         | None     | None               |
+------------------------+-------------------+----------+--------------------+

The field ``Controlled Characters`` lists the simulated characters that the Orge user can control. This field can contain only one character identifier, or a fixed-sized tuple or a list containing character identifiers, depending on the chosen account settings for that Orge instance.

The corresponding Erlang ``record`` is defined in orge_database_manager.hrl_.

To inspect the data in the tables of the Orge database, one just has to launch from the shell::

 > tv:start().
 
 
 
Schema For Simulation State
---------------------------

Schema For Monitored Events
---------------------------


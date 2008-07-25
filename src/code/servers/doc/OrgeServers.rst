

.. role:: raw-html(raw)
   :format: html
   
.. role:: raw-latex(raw)
   :format: latex



.. _orge_tcp_server.hrl: http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/servers/raw-tcp/src/orge_tcp_server.hrl?view=markup

.. _orge_tcp_server.erl: http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/servers/raw-tcp/src/orge_tcp_server.erl?view=markup

.. _orge_tcp_server_test.erl: http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/servers/raw-tcp/src/orge_tcp_server_test.erl?view=markup



.. _orge_tcp_client.hrl: http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/clients/raw-tcp/src/orge_tcp_client.hrl?view=markup

.. _orge_tcp_client.erl: http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/clients/raw-tcp/src/orge_tcp_client.erl?view=markup

.. _orge_tcp_client_test.erl: http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/clients/raw-tcp/src/orge_tcp_client_test.erl?view=markup



.. _orge_client_manager.hrl: http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/servers/functional-services/client-management/src/orge_client_manager.hrl?view=markup

.. _orge_client_manager.erl: http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/servers/functional-services/client-management/src/orge_client_manager.erl?view=markup



.. _orge_database_manager.hrl: http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/servers/functional-services/database-storage/src/orge_database_manager.hrl?view=markup

.. _orge_database_manager.erl: http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/servers/functional-services/database-storage/src/orge_database_manager.erl?view=markup

.. _orge_database_manager_test.erl: http://osdl.svn.sourceforge.net/viewvc/osdl/Orge/trunk/src/code/servers/functional-services/database-storage/src/orge_database_manager_test.erl?view=markup




.. _testbed: orge-testbed.esperide.com


.. _Orge server:
.. _Orge servers:


Orge Servers
============

.. contents:: 
	:local:


Overview
--------


An Orge server is the part of the system which is linking the simulated world to remote peers.

It allows to inject events and retrieve informations from the Orge virtual world, which otherwise would live its life in a black box.

An Orge server must be able to handle multiple clients interacting concurrently with the same universe. 

A logical Orge server can be made of several software servers, instanciated on the same computer or in a set of networked computers (ex: a cluster). It allows to share the load (memory, network and processing power) and to have communicating gateways between a set of simulated worlds.


Some basic client/server concerns must be dealt with:

	- the authentification of clients
	
	- the update of the Orge code without stopping the simulation (hot code reload with preservation of the server state)

	
Notable constraints are:
 	
	- a client behaving incorrectly should not jeopardize the server
	
	- commands affecting the server should be transactional (either completely performed on the server, or completely ignored by it)
	
	- clients have to be controlled, regarding their number (maximum number of clients at a time) and their behaviour (flow control to avoid denial of service)
	
	- there should be a way of propagating and synchronizing resources (ex: media content) between the server baseline versions and each client

	- clients can be heterogeneous, from the debug clients (written in Erlang) to the end-user ones (written in C++, OpenGL, etc., and using notably OSDL)


Interesting properties would be:
	
	- the simulation should be "fair" (regarding models having different needs and clients having different means) and respect causality, even with remote clients and/or a distributed Orge server	

	- the simulation should be reproducible, either on a best effort basis (no strict total ordering enforced), or totally (at the expense of server resources)  

The Orge server acts only as an interface between the clients and the simulated world. Its role is mainly to convert incoming data from the clients into a series of applicative datagrams then commands, and to send regularly updates from the server to each client.  	


Several kinds of servers can be imagined:

	- TCP-based
	
	- UDP-based
	
	- hybrid, for example using UDP for synchronization and TCP for resource uploading


Furthermore each server can be based on:

	- pure custom developments : there are called "raw" servers (ex: ``raw-tcp``)
	
	- reuse of OTP behaviours (ex: ``gen_server``, ``gen_fsm``, etc.)	
	
	
.. Note:: The term *clients* will be used quite often. This must be of course understood here in a client/server context, not as "paying customers".


Sources 	
-------
	
Currently raw TCP Orge servers are used. They are implemented in orge_tcp_server.hrl_ and orge_tcp_server.erl_, and tested in orge_tcp_server_test.erl_.

To a given Orge server instance, any number of Orge clients can connect. Orge test clients are implemented in orge_tcp_client.hrl_ and orge_tcp_client.erl_, and tested in orge_tcp_client_test.erl_.

Each raw TCP Orge server will spawn one client manager per connected client. The client manager is implemented in orge_client_manager.hrl_ and orge_client_manager.erl_.

Each raw TCP Orge server uses an Orge database, implemented in orge_database_manager.hrl_ and orge_database_manager.erl_, and tested in orge_database_manager_test.erl_.



	
	
Setting Up An Orge Server Instance
----------------------------------

The recommended scheme to run an Orge server is to host it on a gateway (ex: a dedicated powerful server, let's name it ``aranor``) and to monitor the simulation from a remote computer (ex: a laptop, let's name it ``rainbow``), linked to the server by a specific private interface:

:raw-html:`<img src=Orge-testbed.png></img>`
:raw-latex:`\includegraphics[scale=0.5]{Orge-testbed.png}`


Server Hardware
...............


Most basic Configuration
________________________

Of course, the more RAM, cores and network bandwidth the server will have, the better (in that order).

However we run our Orge testbed on a Pentium II gateway, running at 300 MHz with 256 megabytes of RAM without noticing unbearable slowdowns.

One element whose importance is often underestimated is the power supply. Investing in a reliable one instead in a bulk one can save you much trouble [#]_. We rely on a `Enermax <http://www.enermax.com>`_ Liberty, and so far it works well. 

Erlang is cross-platform and can run on numerous architectures, but we would recommend to stick to the x86 one for increased safety and lower hardware prices.

Similarly, if 32-bit and 64-bit architectures are both fully supported, for the moment we would however prefer the former to the latter: this is not due to the fear of bugs that could remain in more recent 64-bit ports; the reason is just the increased memory footprint of data in actual 64-bit mode (each pointer used internally occupying twice the size it would occupy in 32-bit). Orge servers tend to be more RAM-bound than CPU-bound.


.. [#] The previous testbed server we used had numerous components (motherboard, hard disk, etc.) destroyed by a faulty no-name power supply. Still better than a fire in your shed or basement, though.



Ideal Configuration
___________________


.. _estimation: dell-server-example.pdf

What could be an ideal configuration to run an Orge server ? At mid-2008, it would be:

  - 8 cores (2 quad cores), since Orge relies on Erlang with can take advantage of SMP and multicores (not to mention distributed servers across multiple computers/clusters)

  - 16 GB of RAM, as virtual worlds need quite a lot data to be described, especially in higher level languages like Erlang
  
  - 3 hard drives, 400GB SAS, in a `RAID-5 <http://en.wikipedia.org/wiki/Standard_RAID_levels#RAID_5>`_ array, for data persistance (it leads to 800GB of usable redondant disk space)
   
  - redundant power supply
  
  - two gigabit ethernet network interfaces

It would cost roughly 5,000 $ (3,200 euros), based on an estimation_ from DELL. Quite expensive for hobbyists of course, but rather affordable for professionals.

Some intermediate configurations could be considerably less expensive [#]_, by building one's server using mass-market components aiming at the power-users, instead of seeking solutions in the professional market.


.. [#] Not to mention destocking operations. As an added bonus, dealing with non-cutting edge technologies leads generally to less trouble when installing and customizing the operation system (notably in the case of GNU/Linux).



Alternate Server Configurations
_______________________________


Distributed Servers
*******************

One of them is to rely on *distributed servers*, either organized as a cluster (generally several similar computers in the same location, linked by a high-speed local network) or really distributed across the Internet (in this case with a looser coupling). 



Third-Party Hosting Solutions
*****************************

Numerous companies providing hosting based on dedicated game servers exist (ex: `1 <http://www.theplanet.com>`_, `2 <http://www.gameservers.com>`_, `3 <http://www.onlinegameservices.com>`_, etc.). Many more provide generic-purpose dedicated hosting as well.

Main advantages should be simplicity, flexibility/scalability, guaranteed quality of service and support.

Main drawbacks are the high costs and, from the point of view of the technical guy, the regret of not having mastered these tough, yet rewarding, challenges.

Other temporary solutions could be :

	- one of the newer inexpensive dedicated offers based on low-end but dedicated servers, like the ones provided (at least in France) by `OVH <http://www.ovh.com>`_ and `Dedibox <http://www.dedibox.fr>`_.

	- a guaranteed portion of a powerful virtualized (shared) server, like the very cheap and flexible `Gandi <http://www.gandi.net/hebergement/>`_ offer (in French) 
 


Optional: Uninterruptible Power Supply
______________________________________


To avoid a cause of downtime, using some kind of `Uninterruptible Power Supply <http://en.wikipedia.org/wiki/Uninterruptible_power_supply>`_ (UPS) is advised.

As for us, we use a *1200 VA Belkin Universal UPS* (cost: 173 euros in 2005) and we are quite happy of it [#]_, the server can last for about 10 minutes waiting for the external supply to return. 


.. [#] Note nevertheless that the lead-acid batteries are detrimental to the environment and must be recycled. Beware to their delivery as well, as ours weighted more than 13 kg.


It also somewhat useful protect your server from all common power problems, including voltage surges (for a nominal 230V, tension excursion can be in the 205-235V range).


Using some kind of monitoring is strongly recommended: thanks to `Network UPS Tools <http://networkupstools.org/>`_ (NUT) and to a tiny script (ex: `displayUpsStatus.sh <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/shell/displayUpsStatus.sh?view=markup>`_) one can ensure his UPS is up and running::

   > displayUpsStatus.sh
   battery.charge: 100
   battery.voltage: 27.3
   battery.voltage.nominal: 24
   driver.name: belkinunv
   driver.parameter.port: /dev/ttyS1
   driver.version: 2.0.4
   driver.version.internal: 0.06
   input.frequency: 50.1
   input.frequency.nominal: 50
   input.sensitivity: normal
   input.transfer.high: 264
   input.transfer.low: 187
   input.voltage: 222.5
   input.voltage.maximum: 236.6
   input.voltage.minimum: 205.1
   input.voltage.nominal: 230
   output.frequency: 50.1
   output.voltage: 221.7
   ups.beeper.enable: yes
   ups.firmware: 2
   ups.load: 0
   ups.model: F6C120xxUNV
   ups.power.nominal: 1200
   ups.status: OL CHRG
   ups.test.result: no test performed
   ups.timer.restart: 0
   ups.timer.shutdown: 0
   ups.type: OFFLINE


Do not forget to link all the necessary devices for operation to the UPS supply (the server, but also any modem, switch, etc.), otherwise the server state would be preserved, but not the continuity of service, as clients would not be able to connect anymore.


More advanced configurations allow to:

	- be notified of abnormal situations (by mail, SMS, etc.)
	
	- trigger a clean server shutdown if the batteries run to low during an outage



Optional: RAID Array
____________________

Running a virtual world involves manipulating large volume of data. As all data will not fit in RAM, and should be preserved from all kinds of crashes (ex: user informations, states of simulated elements, etc.), they must somehow be stored in non-volatile storage. Yet they must remain readily available, within a few moments.

As long as solid-state disks will not be generalized (still low capacity, high prices, etc.), we have to rely on hard disks. Sadly, failures occur quite frequently with them, and jumping back to the latest backup is not an option (it would be too long to perform, and archives cannot be done frequently enough not to loose significant durations).

So we end up adding a layer of reliability to hard disk with RAID arrays. Getting a performance gain is not the first goal here.

There are many kinds of RAID configurations: they can be done in hardware or software, and disks can be organized in various ways, including according to `standard RAID levels <http://en.wikipedia.org/wiki/Standard_RAID_levels>`_.

For most configurations, a RAID 5 looks like a good compromise between reliability and cost. 

Low-budget Orge server can run on software RAID. Beside purchasing the right disks [#]_, one should consider determining which partitions should be mirrored (data only, or all, including problematic cases as ``/boot``), which actual connectivity (disks may or may not have to share a bus)


.. [#] At least three hard disks are needed for RAID 5, preferably from different manufacters to avoid simutaneous failures, with same size and similar performances. A fourth disk can be useful as a spare disk, to reduce the vulnerability after a failure.

Beyond the setting up of the RAID array, some training is welcome to know exactly what should be done to recover after the crash of a disk.
 
Once again, some tools help, in order to:

 - being able to predict disk failures, using the *Self-Monitoring, Analysis and Reporting Technology System* (SMART). See notably `smartmontools <http://smartmontools.sourceforge.net/>`_ 
 
 - being notified (ex: by mail) whenever a RAID problem occurs
 
 - having a simple way of checking the state of the RAID array (ex: `displayRAIDStatus.sh <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/shell/displayRAIDStatus.sh?view=markup>`_), for example::
 
	Personalities : [raid1]
	md0 : active raid1 hda9[0] hdc3[1]
	      19550976 blocks [2/2] [UU]

	unused devices: <none>
	[dev   9,   0] /dev/md0         3FC30DFE.3243F52C.9AEE4B58.D707F02D online
	[dev   3,   9] /dev/hda9        3FC30DFE.3243F52C.9AEE4B58.D707F02D good
	[dev  22,   3] /dev/hdc3        3FC30DFE.3243F52C.9AEE4B58.D707F02D good

	/dev/md0:
	        Version : 00.90.01
	  Creation Time : Sat May  7 00:48:16 2005
	     Raid Level : raid1
	     Array Size : 19550976 (18.65 GiB 20.02 GB)
	    Device Size : 19550976 (18.65 GiB 20.02 GB)
	   Raid Devices : 2
	  Total Devices : 2
	Preferred Minor : 0
	    Persistence : Superblock is persistent

	    Update Time : Sat May  6 00:27:42 2008
	          State : clean
	 Active Devices : 2
	Working Devices : 2
	 Failed Devices : 0
	  Spare Devices : 0

	           UUID : 3fc30dfe:3243f52c:9aee4b58:d707f02d
	         Events : 0.270524

	    Number   Major   Minor   RaidDevice State
	       0       3        9        0      active sync   /dev/hda9
	       1      22        3        1      active sync   /dev/hdc3
		


Optional: Backup System
_______________________

Once in a while, a full backup of the simulation state should be performed, to resist to "disasters" such as fire, flood, crackers, etc.

This can be done either direcly by writing the data to a dedicated hard disk that will be then removed and stored in a secure place, or, more usually, by burning a removable media.
 
Knowing that a dual layer Blu-ray Disc can store 50 GB (almost six times the capacity of a dual layer DVD) and that they are already affordable (and their discs too), most virtual worlds, once their state efficiently compressed (ex: using ``bzip2``), should be archived that way.




Network Needs
.............



Network Connection
__________________


When not relying on third-party hosting, the connection of the Orge server to the Internet thanks to an ISP is often the weakest part of the setup.

The very basic needs regarding this connection are:
 
  - it must be permanent, so that clients can connect to the persistent server at any time (no dial-up, of course)
  
  - it must be based on a static (fixed) IP address: otherwise, even if a DNS updater assign dynamically a changing IP to your server DNS name, new clients will keep on accessing outdated DNS informations for a while (until the update has been widely propagated in the worlwide DNS) whereas current clients will have to change IP immediately without notice. This would be complex and unreliable
  
  - IPv4 or IPv6 do not really matter; and one IP address is enough
  
  - a high bandwidth is needed, in both directions, and preferably symmetrical [#]. Hopefully traffic will be unlimited, otherwise even high thresholds could be reached
  
  - the higher quality of service, the better. This includes low lag, permanent monitoring, redundancy, guaranteed high availability, etc.


.. [#] Upstream bandwidth is at least as important as downstream one when running Orge servers, whereas connections like ADSL ones asymmetrical by nature (ex: download bandwith more than 5 times greater upload one).


The choice of a good ISP is thus fundamental. There are benchmarks available for the major ones.

Once the ISP is chosen, one should properly configure its connection: beyond ensuring the Internet access is reliably working, often an administration console is provided. 

A careful examination of the settings might allow to tune it a bit, regarding reverse DNS, ping fastpath, etc.

Storing its settings and its measured performances (by the ISP and by third-party tests) is surely a good practise. 

For example for an ADSL connection::

	DSLAM ***** ligne 2 / 5 / 10  (3)
	Uncompatible line conditions 	0
	Unselectable operation mode 	0
	Spurious atu detected 	0
	No lock possible 	0
	Forced silence 	0
	Protocol error 	0
	Timeout 	0
	Attainable bitrate 	945 kb/s (up) 	7340 kb/s (down)
	Capacity occupation 	0 (up) 	0 (down)
	Chan data Interleave 	6039 (far) 	945 (near)
	Chan data Fast Path 	0 (far) 	0 (near)
	Attenuation 	27 dB (up) 	50 dB (down)
	Noise margin 	7 dB (up) 	7 dB (down)
	Output power 	6 (up) 	18 (down)
	      Interleave
	  Fec 	338698043 (far) 	1463 (near)
	  Hec 	673 (far) 	0 (near)
	  Crc 	816 (far) 	0 (near)
	  Rx cells 	3648497
	  Tx cells 	3657001
	      Fast Path
	  Fec 	338698043 (far) 	1463 (near)
	  Hec 	673 (far) 	0 (near)
	  Crc 	816 (far) 	0 (near)
	  Rx cells 	3648497
	  Tx cells 	3657001
	Interleaved profile required for G-DMT lite 	0
	Requested bitrate too high for G-DMT lite 	0

which results in these measures::


	Download stream: 5,01 Mbit/s (641,45 KB/s)
	Upload stream: 934,16 kbit/s (116,77 KB/s)





Network Addresses & DNS
_______________________




Domain Name
***********


One key point of running a persistent server on the Internet is to have it respond to a name rather than an IP, to provide an additional indirection level (changing ISP should be transparent for the clients) and so that humans can share more easily the way of accessing to the Orge server (its URL).

The first thing is thus to register a domain name. As they are inexpensive (about 15 euros per year, depending on the chosen extension), this is a must. For example, we are making use of ``esperide.com``, that we reserved with great satisfaction thanks to already praised `Gandi.net <http://www.gandi.net/>`_.



DNS Registering
***************


Once one bought a domain name, he must associate DNS informations to it, so that the various Internet traffic (web, mail, etc.) is routed to the desired IP address. 

Most of the time the company you bought your domain name from provides these DNS services. This is the case of Gandi.net, but we prefer currently using the - free - services of another DNS service, `ZoneEdit <http://zoneedit.com/>`. 

We chose them because:

  - their services remain free until 200 MB of DNS transfer is met during a year (1 million DNS queries), which should seldom occur due to DNS caching from clients
  
  - their DNS web interface is one of the simpliest to configure (most others are rather tricky)
  
  - they are believed to be among the most robust DNS providers on the Internet, and they are probably located not in Europe only


Should we exceed 1 million DNS queries a year, we would probably switch back to Gandi DNS.


Another interesting feature is to have the reverse DNS configured. This setting must be checked with your ISP, not with your domain name or DNS service providers. It allows to associate your IP address to your domain name, instead of having it associated to a domain of your ISP.




Server Software
...............


Operating System
________________


Orge is implemented in Erlang, which is, as already mentioned, supported by numerous platforms.

The operation system we would elect is the GNU/Linux one, for stability, ease of use and administration, performances and available support.

Among the various distributions, for the Orge server latest Debian stable seems a good choice.

For the monitoring client, choice is less crucial, but we would favour a Ubuntu distribution, for ease of installation and user-friendliness.

Having a server directly exposed to the Internet requires a properly configured firewall, as discussed below, but also the use of the safest software. This includes favouring stable versions over cutting-edge ones, but also, somewhat contrarily,  integrating quickly any available security update.

A simple automatic updater of stable versions and security patched ones is easy to set-up on Debian-based distributions, see for example our `debian-updater.sh <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/shell/debian-updater.sh?view=markup>`_ script, to be placed for example in ``/etc/cron.weekly``.



Disk Settings
_____________


Running an Orge server implies reserving some storage space for the application and its data.

The disk itself (speed, interface type, model, etc.) is a significant parameter, and its configuration matters a lot (see the ``hdparm`` tool).

The needed space should be ideally placed on dedicated partitions, using carefully selected filesystem types. Knowing we are to deal mostly with few large files and that integrity is essential, we would go preferably, instead of the `ReiserFS <http://en.wikipedia.org/wiki/ReiserFS>`_ filesystem and others, for:

	- the `Ext3 <http://en.wikipedia.org/wiki/Ext3>`_ filesystem, with the ``Journal`` level of journaling, as opposed to less reliable ``Ordered`` and ``Writeback`` levels [#]_
	
	- the `XFS <http://en.wikipedia.org/wiki/XFS>`_ filesystem


.. [#] See ``tune2fs`` and the ``journal_data`` option



Storage For The Orge System Itself
__________________________________


As not all software resources are to be installed directly in the system tree (most Orge prerequesites should better be configured and installed by hand with proper versions), some space must be left for these software elements.
 
If ever they were lost due to a disk failure, they could be recreated quite easily, so having them on a RAID partition and/or being archived is less necessary than it would be for simulation data [#]_.


.. [#] Knowing that simulation data ought in all cases to benefit from these safety measures, the added cost of including also the application itself is quite often negligible.

Reserving 1GB for these needs should be enough.



Application Static Data
________________________

These are application-specific, they include for example the static game content that should be streamed to all clients.

As the sources of these data are most probably stored elsewhere, the loss of the ones installed in an Orge instance should not a be a problem. Therefore the same policy as the one for the Orge system should apply.



Application Dynamic Data
________________________


This is obviously the core of the data that should not be lost. Redundancy though RAID storage and regular backups are a must here.

These data are mainly composed of:
	
	- Orge server configuration (ex: network ports, versions and paths being used)

	- user settings (ex: logins/passwords)
	
	- simulation-specific Orge persistance files, i.e. generally snapshots of the full game state


Depending on size, richness, number and depth of simulated elements, history, etc., the last item, simulation data, can grow quite a lot. 

A relatively high upper bound for them would be 20GB.




Users, Groups And Permissions
_____________________________


Apart the administration tasks already mentioned (network configuration, system updates, etc.), no root access is stricly needed to set-up or run an Orge server. For example, Orge servers can run in non-privileged ports.

For managibility as well as security reasons, creating user and group that are Orge-specific is strongly recommended.

This can be done that way::

	> adduser --system --group orge
	Adding system user `orge' (UID 107) ...
	Adding new group `orge' (GID 107) ...
	Adding new user `orge' (UID 107) with group `orge' ...
	Creating home directory `/home/orge' ...                                    


Some checkings can be made::
	
	> id orge
	uid=107(orge) gid=107(orge) groups=107(orge)

	> grep orge /etc/passwd
	orge:x:107:107::/home/orge:/bin/false
    
	> grep orge /etc/shadow
	orge:!:14031:0:99999:7:::

	> tree /home/orge/
	/home/orge/

	0 directories, 0 files


This setting forbids login with the ``orge`` user and access to a shell by using ``su``. 

As nevertheless it may be convenient to be logged as ``orge``, one can issue ``chsh orge`` to specify a real shell or use directly ``adduser`` with the ``--shell``. This way, root (only) can switch, once logged, to the ``orge`` user. The recommended way is to use from root ``su orge`` rather than ``su - orge``, to ensure any shell configuration file (ex: ``/home/orge/.bashrc``) will be parsed.

In this example we will be installing Orge in ``/home/orge/``, using as prefix```/home/orge/software``.



Firewall
________


We are using the `Netfilter <http://www.netfilter.org/>`_ firewall, also known as ``iptables``.

We provide a complete and fully commented iptable configuration script, customized for the hosting of Orge servers, `iptables.rules-Gateway.sh <http://ceylan.svn.sourceforge.net/viewvc/ceylan/Ceylan/trunk/src/code/scripts/shell/iptables.rules-Gateway.sh?view=markup>`_.

The ``Orge section`` is the part we are interested in here [#]_:

  - a TCP port (4369) is left open for the ``epmd`` daemon
  - a TCP port range (51000 to 51999) is left open for dynamically created Erlang connections, knowing that the interpreter must be given this range (thanks to the ``inet_dist_listen_min/max`` kernel option)
  

.. [#] More informations about Erlang and firewalling issues can be found in this `article <http://www.bluishcoder.co.nz/2005/11/distributed-erlang-and-firewalls.html>`_.


We did our best to create the most secure rules, any feedback would be appreciated. Of course, depending on the configuration and the services that run on your server, this script should be customized accordingly (ex: regarding the name of the network interfaces). Only SSH login is allowed. 


With the basic default configuration, an Orge server has to open various ports, listed below. 


Main TCP listening port
***********************

It handles incoming client connections, which will result in the creation of a short-lived per-client TCP socket, dedicated to administration (ex: client authentification) and data streaming (ex: downloading of newer simulation resources).

This main TCP listening port is set by default to ``9512``, see orge_tcp_server.hrl_.

It is autorized by the firewall thanks to::

	# For the listening socket of TCP Orge server:
	iptables -A INPUT -p tcp --dport 9512 -m state --state NEW -j ACCEPT



Per-client TCP ports
********************

Once the previous listening TCP socket accepted a new connection, a socket dedicated to exchanges with this client is opened by the server.

All these per-client TCP sockets are in a port range (default: ``51000-51999``, see orge_tcp_server.hrl_)::

	# For client TCP Orge server sockets:
	iptables -A INPUT -p tcp --dport 51000:51999 -m state --state NEW -j ACCEPT

Thus by default up to ``51999-51000+1 = 1000`` connected TCP clients are allowed at a time. Note however that many more clients can interact with the simulated world, as nominal communications (not administration, not streaming) are performed over UDP ports, with no specific upper bound set in the client number.



Main UDP port
*************

It is used by the server to interact with all clients, sending them world updates, and receiving from them newer commands issued. 

It implies the following rule:;

	# For main UDP Orge server sockets:
	iptables -A INPUT -p udp --dport 9512 -m state --state NEW -j ACCEPT


The same port number can be used both for the TCP listening socket and for the main UDP port, as TCP and UDP port numbers are completely independent.


The ``netstat`` tool can be used to check local open ports.




Security
________


At the lowest level, security is obtained thanks to the kernel, the corresponding distribution, and the aforementioned firewall settings.

At the highest level, the Orge database allows to authenticate each Orge user thanks to identifiers, and to report through supervision traces every abnormal connection attempt.

In between lies the Erlang environment, with strict laws regarding the interconnecion of virtual machines and processes.



Erlang Cookie
*************

It allows to choose which 

The recommended way is to set the same Erlang cookie in the Orge server(s) and every computer allowed to connect to the Orge instance (say, on the laptop of an Orge admin).

This can be done that way [#] _::

.. [#] Of course other cookie sentences should be used instead.

 echo 'This is my Orge cookie.' > ~/.erlang.cookie
 chmod 400 ~/.erlang.cookie
 
 
Then that cookie shall be transferred (ex: thanks to ssh) to every computer to be connected with. Check that the owner and permissions are correct (ex: ``-r-------- 1 orge orge``).

The cookie can be checked from an Erlang shell: use ``erlang:get_cookie().``.

An host file (see ``net_adm:host_file``) can be used as well::

  > cat  ~/.hosts.erlang
  'aranor.esperide.com'.
  'sonata.esperide.com'.
  'rainbow.esperide.com'.
  
Then ``net_adm:world(verbose)`` can be used to detect all accessible nodes.
  

Remote Connection to the Orge Server Erlang Shell
*************************************************

This is a convenient way of manipulating an Erlang shell remotely. 

See `Interconnecting Erlang Nodes <http://www.ejabberd.im/interconnect-erl-nodes>`_ for further details.

Another maybe more interesting solution is to use SSH to log-in to the server, create a local shell and use it to communicate (locally) with the server one.



Tool Versions
_____________

Both Orge servers and monitoring clients should run the lastest stable release of Erlang and Orge.

More precisely, the following set of software and data should be kept up to date:

	- the running operating system (ex: Debian, including the Linux kernel and all packages)
	- the latest custom stable build of Erlang/OTP environment
	- the latest custom stable build of the Orge server
	- the `egeoip` module and the GeoLite database, for IP geolocation

Erlang and other prerequesites should be configured, compiled and installed specifically for the Orge needs, with relevant settings. See the `Installation Thanks To LOANI`_ section.


Installation Thanks To LOANI
____________________________

LOANI allows to download, build, install, link together a set of Ceylan and OSDL developments, including Orge.

In the context of an Orge server, once some base tools have been installed (ex: flex), one may download `latest LOANI archive <http://sourceforge.net/project/showfiles.php?group_id=71354&package_id=161367>`_ and for example run from the LOANI-x.y extracted directory with the ``orge`` user::

  ./loani.sh --onlyOrgeTools --prefix /home/orge/software


LOANI will take care of Erlang, egeoip and GeoLite.

Note that the development package for libcurses is needed to build Erlang. On Debian and Ubuntu, the corresponding package is ``libncurses5-dev``, to be installed before running LOANI.

This leads to following output::

 orge@myserver:~/LOANI-0.5$  ./loani.sh --onlyOrgeTools --prefix /home/orge/software


         < Welcome to Loani >

 This is the Lazy OSDL Automatic Net Installer, dedicated to the lazy and the fearless.

 Its purpose is to have OSDL and all its pre requesites installed with the minimum of time and effort. Some time is nevertheless needed, since some downloads may have to be performed, and the related build is CPU-intensive, so often a bit long. Therefore, even with a powerful computer and broadband access, some patience will be needed.
 Retrieving all pre requesites, pipe-lining when possible.
 Target package list is <Erlang egeoip Geolite >.
 Some tools already available (Erlang Geolite), others will be downloaded (egeoip).
 	   <---- egeoip retrieved [from SVN]
 All pre requesites available.
 	   ----> Erlang 	 : extracting [OK] configuring [OK] building [OK] installing [OK]
 	   ----> egeoip 	 : extracting [OK] configuring [OK] building [OK] installing [OK]
	   ----> Geolite	 : extracting [OK] configuring [OK] building [OK] installing [OK]
 Post-install cleaning of build trees.
 End of LOANI, started at 09:01:00, successfully ended at 09:46:59.
 
 
A simple shell configuration file is generated by LOANI, and can be applied to the Orge user environment, for example::  

 orge@myserver:~/LOANI-0.5$ cat /home/orge/software/Orge-environment.sh >> /home/orge/.bashrc
 orge@myserver:~/LOANI-0.5$ cat ~/.bashrc
 # This is the Orge environment file.
 # It has been generated by LOANI on July 2008, 12 (Saturday).
 # Source it to update your environment, so that it takes into
 # account this LOANI installation.
 # PATH and LD_LIBRARY_PATH will be automatically updated accordingly.
 #
 # Usage example:
 # . /home/orge/software/OSDL-environment.sh
 # This script can be also appended to a shell configuration file. 
 # Ex: 'cat /home/orge/software/OSDL-environment.sh >> ~/.bashrc'. 
 echo "--- Orge Settings File sourced ---"

 # Erlang section.
 Erlang_PREFIX=/home/orge/software/Erlang-R12B-3
 export Erlang_PREFIX
 PATH=$Erlang_PREFIX/bin:${PATH}
 LD_LIBRARY_PATH=$Erlang_PREFIX/lib:${LD_LIBRARY_PATH}



Managing An Orge Server Instance
--------------------------------
 

Administration
..............

A special Orge client exists for server administration: the orge_admin module.

stop/shutdown/deconnection


Monitoring Data
...............
 

Connections to the Orge server - whether they are successful or not - are traced and stored in database.

Several informations about each incoming client are gathered:
 
 - the client IP address
 - the reverse DNS corresponding to this IP address
 - the login associated with this connection
 
See `Schema For Monitored Events`_ for more details.


Supervision
...........

When running an Orge server instance, traces are stored in a local file by the aggregator, so that nothing is lost in case the communication link is lost.

However usually an administrator wants to be able to have access to traces in real-time from another host, for example the remote administrator's laptop.


Trace Monitoring
________________


A running Orge server records traces of the main events of interest regarding its operation. Distributed traces are collected (in the case where the Orge server is running on several nodes), and stored locally, in a file respecting a given trace format. For that a collection of WOOPER classes - TraceEmitter, TraceAggregator, TraceSupervisor - are readily available.

A specific Java parser has been developed so that the `LogMX <http://www.logmx.com/>`_ log file visualizer can interpret directly these traces, and become the recommended TraceSupervisor.

However this would allow only for local supervision (i.e. supervision from the server Orge is running on). 

An additional feature is provided by Orge: while letting the reference trace file on the server (if stored elsewhere it could be affected by network problems), any number of Orge Trace Listeners can connect to the server and have all traces delivered to them: past traces are automatically gathered, compressed and sent by the Orge server to a new Trace Listener, whereas new traces are directly sent afterwards to each connected listener.

A typical use-case is an Orge admin connecting simply from its laptop to an Orge server, analyzing the traces, and deconnecting when having finished, without further interferences on the server. 


Geolocation
___________

Geolocation informations can be deduced from the IP address of Orge clients.
 
They are obtained thanks to the `egeoip <http://code.google.com/p/egeoip/>`_ module using the `GeoLite City <http://www.maxmind.com/app/geolitecity>`_ location database, proposed free of charge by `MaxMind <http://www.maxmind.com>`_.

The pair was found to working very well, returning very accurate look-ups::

 1> egeoip:start().
 {ok,<0.33.0>}
 2> egeoip:lookup("82.225.152.215").
 {ok,{geoip,"FR",
 			"FRA",
 			"France",
 			<<"A8">>,
 			<<"Sèvres">>,
 			<<>>,
 			48.8167,
 			2.20000,
 			0,
 			0}}
 3> egeoip:lookup("192.168.0.8").
 {ok,{geoip,[],[],[],<<>>,<<>>,<<>>,601.517,601.519,0,0}}
 4> egeoip:lookup("64.233.187.99").
 {ok,{geoip,"US",
 			"USA",
 			"United States",
 			<<"CA">>,
 			<<"Mountain View">>,
 			<<"94043">>,
 			37.4192,
 			-122.057,
 			650,
 			807}}



Orge Lightweight Client
.......................

This Orge client is developed in Erlang, and can satisfy two goals:

 - provide a MUD-like interface, for narrow-band access and nostalgic players (if any)
 - help debugging the Orge Heavy Client


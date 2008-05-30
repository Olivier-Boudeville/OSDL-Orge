.. _Orge server:
.. _Orge servers:


.. role:: raw-html(raw)
   :format: html
   
.. role:: raw-latex(raw)
   :format: latex


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
	
	

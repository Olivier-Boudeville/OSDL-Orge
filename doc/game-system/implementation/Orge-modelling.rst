
:raw-latex:`\pagebreak`


A More Formal Modelling
=======================


This section describes more precisely how the main concepts used by Orge are modelled.

 - Element
 
   - ActiveElement
   
     - Creature: corresponds to all beings, monsters or characters
	 
	   - belongs to a Species
	   - Has an age and a life expectancy at births
	   - Has a gender
	   - has an Inventory
       - Experience Gain if killed by the player
       - Links to possible completion of quest element
	   - onKilled
	   - onSurrender (if appropriate)
	   
     - Clock: trigger based on (simulation) time	   
   - PassiveElement
   
     - Object: corresponds to all  
	 
	   - onPickUp: might trigger actions (ex: raise in statistics, curse, traps, etc.)
	   - onDrop
	   - onUse (if appropriate)
	   - onUseWith (if appropriate)
	   - contains (if appropriate)
	   
     - Challenge: corresponds to all kind of (generally rewarded) actions
	 
	   - Quest: composed of set of Challenges (C1, C2, .., Cn), whose overall completion is defined with regard to the ones of challenges. For example, a quest could be completed iff C1 and C2 are completed, or if C3 is completed
	   
     - Area: an in-game portion (horizontal plane surface) of the map. It can defined as a (convex or concave) polygon, or as a simple disc
	 
	   - RespawnArea: an area in which a given set of kinds of creature appears at a given rate. Maximum populations can be capped. Ex: ``[ {Troll, ]`` 	   
   

.. Note:: Many elements passive by nature (ex: a challenge, an area, etc.) can be nevertheless turned into active elements on a per-instance basis, if associated to a given Clock object.




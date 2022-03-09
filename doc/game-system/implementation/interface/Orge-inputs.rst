
.. _input handling:


Inputs
......


In General
__________


For all character-based input methods that are poorly user-friendly (ex: the ones of the Nintendo DS), besides abbreviations, a predictive dictionnary could be used: depending on the previous selected keys, different words are suggested to the user to ease her writing process. 

One example is the dictionnary used by `ScummVM DS <http://scummvm.drunkencoders.com/#pred>`_.

The localisation of inputs must be carefully planned before implementation. For example:

 - the english ``W`` for *West* will become ``O`` for *Ouest* in french
 - punctuations and message lengths greatly vary from a language to another
 - way too many different keyboard layouts are in use in the world 

A first step for sure is to let the user redefine the input settings (ex: key bindings).



Keyboard
________



Keyboard Shortcuts
******************

They should be indicated by a graphical hint (underlined, written in red, with a balloon, etc.).

 - CTRL-Q: spell to release the link to player (quit)
 - CTRL-S: spell to save the game
 - F1: instructions
 - F2: Map (if available)
 - F3: Compass (if available)
 - Tab: labels of buttons and game elements (ex: character names)
 - a: attack/talk to a character
 - e: east
 - f: start/end fight
 - g: get item
 - m: cast a Mage spell
 - n: north
 - p: cast a Priest spell
 - s: south
 - u: use/search an object
 - w: west
 


Virtual Keyboard
****************

For handheld devices (ex: the Nintendo DS), a keyboard can be displayed onscreen, on which letters can be selected.

Otherwise character recognition based of user-input dedicated symbols can be used, if a touchscreen is available.


 
Mouse
_____


Leaving the cursor for some time on an onscreen object triggers the display of some informations about that object (*on mouse over*).


Joystick
________

This input device is not general-purpose enough for the games that Orge supports. Besides its use is not so widespread.


Voice Recognition
_________________

Some commands could be directly issued by the voice of the player. 
A basic voice recognition could be used for that. The use of a microphone is anecdotic among the casual gamers.

	

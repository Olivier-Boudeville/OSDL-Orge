#!/bin/sh


echo "  Setting up language wordsets"


# Prefix section.

chris_pound_base_url="http://www.ruf.rice.edu/~pound"



retrieve()
{

	remote_file="$1"
	
	if [ -n "$2" ] ; then
		result_file="$2.txt"
	else
		result_file=`basename $1`"-words.txt"
	fi

	echo "    Retrieving $result_file (from $remote_file)"
	wget $remote_file --output-document $result_file 1>/dev/null 2>&1
	
	res=$?
	
	if [ ! $res -eq 0 ] ; then
	
		echo "### Error, download of $remote_file failed." 1>&2
		exit $res
		
	fi
	
}



retrieve_from_pound()
{

	source_file="$chris_pound_base_url/$1"
	target_file="$2"
	
	retrieve $source_file $target_file
	
}



# Fantasy section.


# Elf-like subsection.


# J.R.R. Tolkien:
retrieve_from_pound sindarin



# Brute subsection.

retrieve_from_pound klingon



# Giant subsection.

# Giants in the Deep:
retrieve_from_pound giants



# Science-fiction section.

# Alien subsection.

retrieve_from_pound barsoomian



# Horror section.

retrieve_from_pound lovecraftian








# Real languages section.




# Contemporary/modern subsection.



# Albanian:
retrieve_from_pound albanian


# Arabic (female names):
retrieve_from_pound arabic-m arabic-male-names


# Arabic (surnames):
retrieve_from_pound arabic-s arabic-surnames



# Basque (words):
retrieve_from_pound basque


# Basque (female names):
retrieve_from_pound basque-f basque-female-names


# Basque (male names):
retrieve_from_pound basque-m basque-male-names



# Bulgarian (words):
retrieve_from_pound bulgarian


# Chinese (syllables):
retrieve_from_pound chinese



# English (words):
retrieve_from_pound english


# English (surnames):
retrieve_from_pound english-s english-surnames


# English (female names):
retrieve_from_pound english-f english-female-names


# English (male names):
retrieve_from_pound english-m english-male-names



# Estonian (words):
retrieve_from_pound estonian


# German (names, including placenames):
retrieve_from_pound german



# Hindi (female names):
retrieve_from_pound hindi-f hindi-female-names


# Hindi (male names):
retrieve_from_pound hindi-m hindi-male-names



# Indonesian (words):
retrieve_from_pound indonesian


# Italian (words):
retrieve_from_pound italian



# Japanese (female names):
retrieve_from_pound japanese-f japanese-female-names


# Japanese (male names):
retrieve_from_pound japanese-m japanese-male-names



# Jaqaru (words from a language related to Aymara):
retrieve_from_pound jaqaru



# Kakadu (names from a Northern Australian language):
retrieve_from_pound kakadu



# Latvian (female names)
retrieve_from_pound latvian-f latvian-female-names


# Latvian (male names)
retrieve_from_pound latvian-m latvian-male-names



# Malay (words):
retrieve_from_pound malay


# Maori (names, including placenames):
retrieve_from_pound maori



# Modern Greek (female names):
retrieve_from_pound mgreek-f modern-greek-female-names


# Modern Greek (male names):
retrieve_from_pound mgreek-m modern-greek-male-names


# Modern Greek (surnames)
retrieve_from_pound mgreek-s modern-greek-surnames



# Polish (words):
retrieve_from_pound polish


# Russian (placenames):
retrieve_from_pound russian-p russian-placenames



# Spanish (female names):
retrieve_from_pound spanish-f spanish-female-names


# Spanish (male names):
retrieve_from_pound spanish-m spanish-male-names


# Spanish (surnames):
retrieve_from_pound spanish-s spanish-surnames



# Swahili (words):
retrieve_from_pound swahili


# Tamil (words): 
retrieve_from_pound tamil



# Thai (female names):
retrieve_from_pound thai-f thai-female-names


# Thai (male names):
retrieve_from_pound thai-m thai-male-names



# Turkish (words):
retrieve_from_pound turkish






# History subsection.



# Latin (words):
retrieve_from_pound latin



# Viking (female names):
retrieve_from_pound viking-f viking-female-names


# Viking (male names):
retrieve_from_pound viking-m viking-male-names



# Ulwa (words from a Mesoamerican language):
retrieve_from_pound ulwa


# Ancient Egyptian:
retrieve_from_pound egyptian ancient-egyptian



# Ancient Greek (female names):
retrieve_from_pound greek-f ancient-greek-female-names


# Ancient Greek (male names):
retrieve_from_pound greek-m ancient-greek-male-names



# Sumerian (words):
retrieve_from_pound sumerian


# Assyrian (names and places):
retrieve_from_pound assyrian



# Celtic (female names):
retrieve_from_pound celtic-f celtic-female-names


# Celtic (male names):
retrieve_from_pound celtic-m celtic-male-names



# Middle English (words):
retrieve_from_pound mid-english ancient-english-words


# Gothic (words):
retrieve_from_pound gothic gothic-words




# Miscellaneous section.

retrieve_from_pound glorantha

retrieve_from_pound jorune


# Annoying with the numerous '?':
retrieve_from_pound tsolyani




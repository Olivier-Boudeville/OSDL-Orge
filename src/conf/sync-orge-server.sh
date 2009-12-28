#!/bin/sh


USAGE="Synchronizes all Ceylan and OSDL code for an Orge server from a remote reference host."



# General section.

RSYNC=rsync
RSYNC_OPT="--bwlimit=50 -pg -e ssh -vcrlz --exclude=*.beam --exclude=*.pdf --exclude=*.dia --exclude=.svn"


# Source section.

ORGE_SERVER_SOURCE_HOST=rainbow

SOURCE_USER=sye

SOURCE_BASE_PATH=/home/${SOURCE_USER}/Projects/LOANI-0.4/LOANI-repository


# Ceylan section.

SOURCE_CEYLAN_BASE_PATH=${SOURCE_BASE_PATH}/ceylan

SOURCE_CEYLAN_ERLANG_PATH=${SOURCE_CEYLAN_BASE_PATH}/Ceylan/trunk/src/code/scripts/erlang


# OSDL/Orge section.

SOURCE_OSDL_BASE_PATH=${SOURCE_BASE_PATH}/osdl

SOURCE_ORGE_BASE_PATH=${SOURCE_OSDL_BASE_PATH}/Orge/trunk/src/code

ORGE_SERVER_SOURCE_PATH=${SOURCE_ORGE_BASE_PATH}


# Target section.

TARGET_USER=$USER

TARGET_USER_PATH=/home/${TARGET_USER}

TARGET_BASE_PATH=${TARGET_USER_PATH}/orge-running-server

TARGET_CEYLAN_PATH=${TARGET_BASE_PATH}/ceylan/Ceylan/trunk/src/code/scripts
TARGET_ORGE_PATH=${TARGET_BASE_PATH}/osdl/Orge/trunk/src


# Actions.

mkdir -p ${TARGET_CEYLAN_PATH}
mkdir -p ${TARGET_ORGE_PATH}



echo "Synchronizing Orge server code from host ${ORGE_SERVER_SOURCE_HOST} to directory ${TARGET_ORGE_PATH}..."

echo "   + Ceylan"
${RSYNC} ${RSYNC_OPT} ${SOURCE_USER}@${ORGE_SERVER_SOURCE_HOST}:${SOURCE_CEYLAN_ERLANG_PATH} ${TARGET_CEYLAN_PATH}

echo "   + Orge"
${RSYNC} ${RSYNC_OPT} ${SOURCE_USER}@${ORGE_SERVER_SOURCE_HOST}:${ORGE_SERVER_SOURCE_PATH} ${TARGET_ORGE_PATH}

echo "...done"


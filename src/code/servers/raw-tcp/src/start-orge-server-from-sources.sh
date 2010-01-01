#!/bin/sh


epmd_port=4269


USAGE="  Usage: "`basename $0`" [ -d | --debug ]
  Launches an Orge server.
  Options are:
     -d or --debug: run in debug mode (not in the background), instead of in the default production mode
"



previous_server_pid=`ps -edf|grep beam|grep orge_tcp_server_exec|awk '{print $2}'`

if [ -n "$previous_server_pid" ] ; then

	echo "An already running Orge server was detected (PID $previous_server_pid), you must kill it first.
  Example: 'kill -9 $previous_server_pid'" 1>&2

	exit 1

fi




# Needed to launch an EPMD daemon on a non-standard port:
epmd -daemon -port ${epmd_port}

if [ ! $? -eq 0 ] ; then

	echo "Error, unable to launch EPMD daemon on port #${epmd_port}." 1>&2
	exit 10

fi


do_debug=1

if [ "$1" = "-d" ] || [ "$1" = "--debug" ] ; then

	do_debug=0

fi


if [ $do_debug -eq 0 ] ; then

	echo "Starting Orge server in debug mode with EPMD port #${epmd_port}..."

	make orge_tcp_server_exec

	echo "...done"

else

	echo "Starting Orge server in production mode with EPMD port #${epmd_port}..."

	make orge_tcp_server_exec_background

	echo "...done"

fi

#!/bin/sh


previous_server_pid=`ps -edf|grep beam|grep orge_tcp_server_exec_background|awk '{print $2}'`

if [ -n "$previous_server_pid" ] ; then

	echo "An already running Orge server was detected (PID $previous_server_pid), you must kill it first.\n Example: 'kill -9 $previous_server_pid'" 1>&2
	
	exit 1
	
fi


echo "Starting Orge server..."

make orge_tcp_server_exec_background 

echo "...done"


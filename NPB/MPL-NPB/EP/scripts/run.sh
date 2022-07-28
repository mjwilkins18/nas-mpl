#!/bin/bash
#This script runs one or more of the EP benchmarks
#$1 = Class to run (default=S)
#$2 = Number of threads (default=64)

if [[ $# -lt 3 ]]; then

	if [[ $# -eq 2 ]]; then
		./ep_${1} @mpl procs ${2} --
	else
		if [[ $# -eq 1 ]]; then
			./ep_${1} @mpl procs 64 --
		else
			./ep_S @mpl procs 64 --
		fi
	fi

else
	echo "Wrong # of args; expecting 2 (name and #threads)"
fi

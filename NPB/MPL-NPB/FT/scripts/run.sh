#!/bin/bash
#This script runs one or more of the FT benchmarks
#$1 = Class to run (default=S)
#$2 = Number of threads (default=64)
#$3 = Grain size (default=1)

if [[ $# -lt 4 ]]; then

	if [[ $# -eq 3 ]]; then
		./ft_${1} @mpl procs ${2} -- -G $3
	else
		if [[ $# -eq 2 ]]; then
			./ft_${1} @mpl procs ${2} -- -G 1
		else
			if [[ $# -eq 1 ]]; then
				./ft_${1} @mpl procs 64 -- -G 1
			else
				./ft_S @mpl procs 64 -- -G 1
			fi
		fi
	fi
else
	echo "Wrong # of args; expecting 2 (name and #threads)"
fi

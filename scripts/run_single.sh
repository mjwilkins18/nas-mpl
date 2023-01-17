#!/bin/bash

# This script run a single benchmark from either MPL or C Implementations of NAS
# $1 = MPL or C
# $2 = Benchmark name
# $3 = Class size
# $4 = Processes
# $5 = Grain Size (MPL Only)

lang=$1
bench=$2
class=$3
procs=$4
grain=$5

if [[ "$lang" == "MPL" ]] ; then
	cd $(pwd)/NPB/MPL-NPB/$bench
	make run CLASS=$class PROCS=$procs GRAIN=$grain | awk -v lang=$lang -v bench=$bench -v class=$class -v procs=$procs -v grain=$grain\
	'{if($3 == "seconds"){print bench","class","procs","grain","lang":"bench":"class":"procs":"grain":"lang":"$5}}'

elif [[ "$lang" == "C" ]] ; then
	cd $(pwd)/NPB/NPB-C-3.0/bin
	export OMP_NUM_THREADS=$procs 
	./${bench,,}.$class | awk -v lang=$lang -v bench=$bench -v class=$class -v procs=$procs -v grain=$grain\
	'{if($3 == "seconds"){print bench","class","procs","grain","lang":"bench":"class":"procs":"grain":"lang":"$5}}'
else
	echo "Error: Invalid Implementation Language"
fi

#END

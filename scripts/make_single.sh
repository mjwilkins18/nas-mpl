#!/bin/bash

# This script makes a single benchmark from either MPL or C Implementations of NAS
# $1 = MPL or C
# $2 = Benchmark name
# $3 = Class size

lang=$1
bench=$2
class=$3

if [[ "$lang" == "MPL" ]] ; then
	cd $(pwd)/NPB/MPL-NPB/$bench
	make CLASS=$class

elif [[ "$lang" == "C" ]] ; then
	cd $(pwd)/NPB/NPB-C-3.0/$bench
	make CLASS=$class
else
	echo "Error: Invalid Implementation Language"
fi

#END

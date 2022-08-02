#!/bin/bash

# This script makes all benchmarks from both the MPL and C Implementations of NAS


root_dir=$(pwd)
langs=("C" "MPL")
benchs=("CG" "EP" "FT" "IS" "MG")
classes=("S" "W" "A" "B" "C")


for lang in ${langs[@]}
do
	for bench in ${benchs[@]}
	do
		for class in ${classes[@]}
		do
			if [[ "$bench" == "FT" ]] && [[ "$class" == "C" ]] ; then 
				continue
			fi

			./scripts/make_single.sh $lang $bench $class
			cd $root_dir
		done
	done
done

#END

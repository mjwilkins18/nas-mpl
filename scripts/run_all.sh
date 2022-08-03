#!/bin/bash

# This script runs all benchmarks from both the MPL and C Implementations of NAS multiple times


root_dir=$(pwd)
langs=("C" "MPL")
benchs=("CG" "EP" "FT" "IS" "MG")
classes=("S" "W" "A" "B" "C")
procs=(1 4 16 64 128)
grains=(1 4 16 64)

for x in {1..5}
do
	for lang in ${langs[@]}
	do
		for bench in ${benchs[@]}
		do
			for class in ${classes[@]}
			do
				if [[ "$bench" == "FT" ]] && [[ "$class" == "C" ]] ; then 
					continue
				fi
			
				for proc in ${procs[@]}	
				do
					if [[ "$lang" == "MPL" ]] ; then
						for grain in ${grains[@]} 
						do
							./scripts/run_single.sh $lang $bench $class $proc $grain
							cd $root_dir
						done

					elif [[ "$lang" == "C" ]] ; then
						./scripts/run_single.sh $lang $bench $class $proc 1
						cd $root_dir

					else
						echo "Error: Invalid Implementation Language"
					fi
				done
			done
		done
	done
done

#END

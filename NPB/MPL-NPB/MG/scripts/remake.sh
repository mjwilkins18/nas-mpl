#!/bin/bash
#This script re-compiles one or more of the MG benchmarks
#$1 = Class to compile (compiles S if param not given)


if [[ $# -lt 2 ]]; then

	echo -n "Compiling MG class="
	class=S
	if [[ $# -eq 1 ]]; then
		class=$1
	fi
	echo $class
	mpl -codegen native -cc-opt "-O3 -mavx512f" -default-type int64 -default-type word64 -default-type real64 -default-ann 'allowVectorExps true' -default-ann 'allowFFI true' -export-header ffi-help.h mg.mlb ffi.c c_print_results.c c_randdp.c c_timers.c wtime.c

else
	echo "Too many arguments"
fi

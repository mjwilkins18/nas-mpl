#!/bin/bash
#This script re-compiles one or more of the EP benchmarks
#$1 = Class to compile (compiles S if param not given)


if [[ $# -lt 2 ]]; then

	echo -n "Compiling EP class="
	class=S
	if [[ $# -eq 1 ]]; then
		class=$1
	fi
	echo $class
	mpl -codegen native -cc-opt "-O3 -mavx512f" -default-type int64 -default-type word64 -default-type real64 -default-ann 'allowVectorExps true' -default-ann 'allowFFI true' -export-header ffi-help.h ep_${class}.mlb ffi.c timing.c

else
	echo "Too many arguments"
fi

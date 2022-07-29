#!/bin/bash
#This script re-compiles one or more of the IS benchmarks
#$1 = Class to compile (compiles S if param not given)


if [[ $# -lt 2 ]]; then

	echo -n "Compiling IS class="
	class=S
	if [[ $# -eq 1 ]]; then
		class=$1
	fi
	echo $class
	mpl -codegen native -cc-opt "-O3 -mavx512f" -default-ann 'allowVectorExps true' -default-ann 'allowFFI true' -export-header ffi-help.h is_${class}.mlb timing.c ffi.c

else
	echo "Too many arguments"
fi

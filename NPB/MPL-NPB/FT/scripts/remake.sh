#!/bin/bash
#This script re-compiles one or more of the FT benchmarks
#$1 = Class to compile (compiles S if param not given)


if [[ $# -lt 2 ]]; then

	echo -n "Compiling FT class="
	class=S
	if [[ $# -eq 1 ]]; then
		class=$1
	fi
	echo $class
	mpl -codegen native -cc-opt "-O3 -mavx512f" -default-type int64 -default-type word64 -default-type real64 -default-ann 'allowVectorExps true' -default-ann 'allowFFI true' -export-header ffi-help.h ft_${class}.mlb timing.c ffi.c

else
	echo "Too many arguments"
fi

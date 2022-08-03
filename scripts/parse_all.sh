#!/bin/bash

# This script parses all benchmark results
# $1=file to parse

file=$1

awk -F: '{total_time[$1]+=$6; ++count[$1]} END{for (key in total_time) if(key != "") print key","total_time[key]/count[key]}' $file | sort -t, -k1,1 -k2,2 -k3,3n -k4,4n

#END

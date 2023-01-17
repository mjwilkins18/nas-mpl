#!/bin/bash

# This script parses all benchmark results
# $1=file to parse

#Example input line:
#MG,B,64,16,MPL:MG:B:64:16:MPL:132.332928


file=$1

awk -F: '{total_time[$1]+=$7; ++count[$1]; if(max_time[$1]=="") max_time[$1] = $7; else if(max_time[$1] < $7) max_time[$1] = $7; if(min_time[$1]=="") min_time[$1] = $7; else if(min_time[$1] > $7) min_time[$1] = $7;} END{for (key in total_time) if(key != "") print key","total_time[key]/count[key]","min_time[key]","max_time[key]}' $file | sort -t, -k5,5 -k1,1 -k2,2 -k3,3n -k4,4n

#END

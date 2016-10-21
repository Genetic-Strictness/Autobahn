#!/bin/bash

num_cols=`awk 'BEGIN {FS=","} ; END{print NF}' $1`
echo $num_cols

for ((i=1; i<=num_cols; i++))
do
echo $i 
script="binwidth=1\n
        bin(x,width)=width*floor((1.0*x)/width) + binwidth/2.0\n
        set boxwidth binwidth\n
	set yrange [0:]
	\n

	set style fill solid 0.5\n
	plot '$1' using (bin(\$$i,binwidth)):1.0 smooth freq with boxes\n"

	echo -e $script | gnuplot -p
done



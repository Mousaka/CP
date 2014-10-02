#!/bin/bash
#Fortran + pgplot compile script
if [ -z "$1"]; then
	for i in *.f; do
	echo "Compiling all..."
	gfortran -o ${1%%.*}.o $i -L /usr/local/pgplot -lpgplot -lX11
	done
else
	echo $1
	if [ -f $1.f ]; then
		echo exists1 && gfortran -o $1.o $1.f -L /usr/local/pgplot -lpgplot -lX11 && ./$1.o
	elif [ -f $1 ]; then
		echo exists2 && gfortran -o ${1%%.*}.o $1 -L /usr/local/pgplot -lpgplot -lX11 && ./${1%%.*}.o
	else
		echo "Error! Could not compile/run file"
	fi
fi
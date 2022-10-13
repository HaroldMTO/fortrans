#!/bin/sh

fortrans=~/util/fortrans

usage()
{
	printf "
Description:
	Produce the list of differences between 2 Fortran namelists files. \
Differences between added, deleted and moved variables and namelists are \
printed following the 'delta' namelist format.

Synopsis:
	$(basename $0) ficold ficnew [-move] [-h]

Arguments:
	-move: authorize moving variables from another namelist
	-h: display help and terminate normally

Details:
	A moved variable is a namelist variable that is present and unique in both \
'old' and 'new' namelist files. If moving variables is authorized, no moving \
is done if variable is duplicated (ie present in several namelists) in any of \
'old' or 'new' file.

Exit status:
	Non 0 in case of error
	0 if not

Dependencies:
	None

Author:
	H Petithomme, Meteo France - DR/GMAP/ALGO
"
}

ficold=""
ficnew=""
move=FALSE
help=0

if [ $# -eq 0 ] || echo " $*" | grep -q ' -h'
then
	usage
	exit
elif [ $# -lt 2 ]
then
	echo "Error: input files missing" >&2
	exit 1
fi

set -e

type R >/dev/null 2>&1 || module load -s intel R >/dev/null 2>&1

echo "$*" | grep -qE ' -move' && move=TRUE || move=FALSE
R --slave -f $fortrans/namdiff.R --args ficold=$1 ficnew=$2 move=$move

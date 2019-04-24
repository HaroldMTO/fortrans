#!/bin/sh

mitra=~/util/fortrans

usage()
{
	printf "
Description:

Synopsis:
	$(basename $0) ficold ficnew [-move] [-h]

Arguments:
	-move: authorizes moving variables from another namelist
	-h: displays help and terminates normally

Details:
	If moving variables is authorized, no moving is done if variable is present \
in several namelists (duplicated) in the old file. However, if a variable is \
duplicated in the new file but not in the old one, moving is done (this is \
dirty).

Exit status:
	Non 0 in case of error
	0 if not

Dependencies:

Author:
	H Petithomme, Meteo France - DR/GMAP/ALGO
"
}

ficold=""
ficnew=""
move=FALSE
help=0

if [ $# -eq 0 ] || echo $* | grep -q ' -h'
then
	usage
	exit
elif [ $# -lt 2 ]
then
	echo "Error: input files missing" >&2
	exit 1
fi

echo "$*" | grep -qE ' -move' && move=TRUE || move=FALSE
R --encoding="latin1" --slave -f $mitra/namdiff.R --args ficold=$1 ficnew=$2 \
	move=$move

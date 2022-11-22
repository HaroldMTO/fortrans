#!/bin/sh

fortrans=~/util/fortrans

usage()
{
	printf "
Description:
	Apply a namelist 'delta' file to a namelist file/dir

	Note: add, delete and 'move' instructions (from one block to another) are performed.

Synopsis:
	$(basename $0) NAM DNAM NAMOUT [-v] [-h]

Arguments:
	NAM: filename (or dir) of namelist to be modified
	DNAM: filename of a 'delta' (instructions) file
	NAMOUT: filename (or dir, if NAM is a directory) of modified namelists
	-v: verbose mode. If passed twice ('-v -v'), it increases verbosity.
	-h: displays help and terminates normally

Details:
	Namelist blocks and variables in blocks are left unsorted..

Exit status:
	Non 0 in case of error.
	0 if not

Dependencies:
	R software

Author:
	H Petithomme, Meteo France - DR/GMAP/ALGO
"
}

if [ $# -eq 0 ] || echo " $*" | grep -qE ' -h'
then
	usage
	exit
fi

ficin=""
delta=""
ficout=""
verbose=0

while [ $# -ne 0 ]
do
	case $1 in
		-v)
			verbose=$((verbose+1))
			;;
		*)
			if [ -z "$ficin" ]
			then
				ficin=$1
			elif [ -z "$delta" ]
			then
				delta=$1
			elif [ -z "$ficout" ]
			then
				ficout=$1
			else
				echo "$1 : unknown option, ignored" >&2
			fi
			;;
	esac

	shift
done

if [ -z "$ficin" -o -z "$delta" -o -z "ficout" ]
then
	echo "error: mandatory arguments missing:
ficin: '$ficin'
delta: '$delta'
ficout: '$ficout'" >&2
	exit 1
fi

set -e

ls -d $ficin $delta >/dev/null

type R >/dev/null 2>&1 || module load -s intel R >/dev/null 2>&1

R --slave -f $fortrans/namapply.R --args ficnam=$ficin delta=$delta ficout=$ficout \
	verbose=$verbose

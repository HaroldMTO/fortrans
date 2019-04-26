#!/bin/sh

varRE="(HOME|USER|LOGNAME|DISPLAY|LANG|HOST|PATH|TERM|SHELL|LC_\w+|(SLURM|PBS)_\w+)"

usage()
{
	printf "
Description:
	Produce a 'guessed' list of environment variables used in C/C++/Fortran files

Synopsis:
	$(basename $0) PATH... [-h]

Arguments:
	PATH...: one or more files or directory to search
	-h: displays help and terminates normally

Details:
	Files are searched recursively from local directory (grep -r .)
	Searched files are restricted to these extensions: .c, .cc, .f90 and .F90
	Listed environment variables exclude those matching (as words):
	'$varRE'

Exit status:
	Non 0 in case of error
	0 if not

Author:
	H Petithomme, Meteo France - DR/GMAP/ALGO
"
}

if [ $# -eq 0 ] || echo $* | grep -q '\-h'
then
	usage
	exit
fi

set -e

temp=$(mktemp)

grep --include=*.[fF]90 -irE "^\s*[^\!]*call +get_?env\w*\( *([\'\"])\w+\1" $* |\
	sed -re 's:.*get_?env\w*\( *\W?(\w+).+:^\1=:i' > $temp

grep --include=*.c  --include=*.cc -irE '\<getenv\w*\( *"\w+"' $* | \
	grep -vE '/\*.*\<getenv' | sed -re 's:.*getenv\w*\( *"(\w+)".+:^\1=:i'>>$temp

sort -u $temp | grep -vwE "$varRE"
rm $temp

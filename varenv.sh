#!/bin/sh

varRE="HOME|USER|LOGNAME|DISPLAY|LANG|HOST|ARCH|PATH|TERM|SHELL|(LC|SLURM|PBS)_\w+"

usage()
{
	printf "
Description:
	Produce a 'guessed' list of environment variables used in C/C++/Fortran \
files.

Synopsis:
	$(basename $0) PATH... [-h]

Arguments:
	PATH...: one or more files or directory to search for
	-h: displays help and terminates normally

Details:
	Files are searched recursively from local directory (grep -r .).
	Files searched are restricted to extensions c, cc, f90 and F90.
	Listed environment variables exclude those matching (as words):
	'$varRE'

Exit status:
	Non 0 in case of error
	0 if not

Author:
	H Petithomme, Meteo France - DESR/GMAP/ALGO
"
}

if [ $# -eq 0 ] || echo " $*" | grep -q '\-h\>'
then
	usage
	exit
elif echo " $*" | grep -q ' \-'
then
	echo "Error: paths starting with '-' are not authorized" >&2
	exit 1
fi

{
	grep --include=*.[fF]90 -irE "^\s*[^\!]*call +get_?env\w*\( *([\'\"])\w+\1" $* |\
		sed -re 's:.*get_?env\w*\( *\W?(\w+).+:^\1=:i'

	grep --include=*.c  --include=*.cc -irE '\<getenv\w*\( *"\w+"' $* | \
		grep -vE '/\*.*\<getenv' | sed -re 's:.*getenv\w*\( *"(\w+)".+:^\1=:i'
} | sort -u | grep -vwE "$varRE"

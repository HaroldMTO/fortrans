#!/bin/sh

usage()
{
	printf "
Description:
	Produce 'empty' concatenated Fortran namelist from Fortran 90 files, \
supposed to contain namelist declarations.

Synopsis:
	$(basename $0) [-ext EXT] [-nofile] [-h]

Arguments:
	-ext : file extension for searching Fortran files containing namelists (as file.EXT)
	-nofile: find files by file extension only (option '-ext' mandatory)
	-h: displays help and terminates normally

	Files produced are:
		- namnul.txt: empty concatenated ordered list of namelists
		- namelists.f90: list of all found namelist declarations, as a Fortran \
file. Note: namelist names and variables are sorted out.

Details:
	Files are searched recursively from local directory (find).
	Files are assumed to all be Fortran code files.
	Found files are rewritten by rewrite.sh before namelist extraction from \
Fortran code files.

Exit status:
	Non 0 in case of non unique namelist names or any else error.
	0 if not

Dependencies:
	Utility tool rewrite.sh (parsing and rewriting of Fortran 90 files).

Author:
	H Petithomme, Meteo France - DR/GMAP/ALGO
"
}

ext=""
files=1
help=0
keep=0

while [ $# -ne 0 ]
do
	case $1 in
		-ext)
			ext=$(echo $2 | sed -re 's:^\.::' -e 's:([^\])\.:\1\\.:g')
			shift
			;;
		-nofile)
			files=0
			;;
		-h)
			help=1
			;;
		-keep)
			keep=1
			;;
		*)
			echo "$1 : unknown option, ignored" >&2
			;;
	esac

	shift
done

if [ $help -eq 1 ]
then
	usage
	exit
elif echo "$ext" | grep -qE "\s+."
then
	echo "Error: spaces found in file extension (invalid)" >&2
	exit 1
fi

set -e

tmpdir=$(mktemp -d tmpXXX)
trap '[ $keep -eq 0 ] && rm -r $tmpdir' EXIT

rm -f namelists.f90 namnul.txt

if [ $files -eq 1 ]
then
	echo "Looking for local files with namelist declaration"
	grep -iErl '^\s*namelist */' > $tmpdir/namfiles.lst
fi

if [ -n "$ext" ]
then
	echo "Looking for local files with extension '*.$ext'"
	# mindepth avoids current dir '.', as sed would make it a blank pattern for grep
	find -mindepth 2 -type f -name \*.$ext -printf "%h/\n" | sed -re 's:^\./::' | \
		sort -u > $tmpdir/namdir.lst

	if [ -s $tmpdir/namfiles.lst ]
	then
		grep -vf $tmpdir/namdir.lst $tmpdir/namfiles.lst > $tmpdir/namfiles.tmp
		mv $tmpdir/namfiles.tmp $tmpdir/namfiles.lst
	fi
fi

if [ -s $tmpdir/namdir.lst ]
then
	echo "Rewrite $(wc -l $tmpdir/namdir.lst) Fortran dirs"
	while read dd
	do
		mkdir -p $tmpdir/$dd
		rewrite.sh -i $dd -o $tmpdir/$dd -ext "$ext" -tabs 0 >> $tmpdir/rewrite.log
		# only 'namelist' (not guaranteed...)
		grep -ihE '^\s*namelist\s*/\s*\w+\s*/' $tmpdir/$dd/*.$ext | \
			sed -re 's: *::g' -e 's:!.*::'
	done < $tmpdir/namdir.lst > $tmpdir/nam.f90
fi

if [ -s $tmpdir/namfiles.lst ]
then
	echo "Rewrite $(wc -l $tmpdir/namfiles.lst) Fortran files"
	mkdir -p $tmpdir/namfiles
	while read ff
	do
		rewrite.sh -i $ff -o $tmpdir/namfiles -tabs 0 >> $tmpdir/rewrite.log
		# only 'namelist' (not guaranteed...)
	done < $tmpdir/namfiles.lst

	grep -hiEr '^\s*namelist\s*/\s*\w+\s*/' $tmpdir/namfiles | \
		sed -re 's: *::g' -e 's:!.*::' >> $tmpdir/nam.f90
fi

if [ ! -s $tmpdir/nam.f90 ]
then
	echo "--> no namelist found"
	exit
fi

echo "Ordering variables in $(wc -l $tmpdir/nam.f90) namelists"
# sort, without -u
while read nam
do
	pre=$(echo $nam | sed -re 's:^(.+/.+/).+:\1:')
	vars=$(echo $nam | sed -re 's:^.+/.+/::' | tr ',' '\n' | sort | xargs | \
		tr ' ' ',')
	echo "$pre$vars"
done < $tmpdir/nam.f90 | sort > namelists.f90

echo "Writing default namelist, ie 'empty namelist' (namnul.txt)"
sed -re 's:^namelist/(\w+)/.+:\&\U\1/:' namelists.f90 > $tmpdir/namnul.txt

echo ". nb of namelists : $(wc -l $tmpdir/namnul.txt)"

uniq $tmpdir/namnul.txt > namnul.txt
if [ $(cat namnul.txt | wc -l) -ne $(cat $tmpdir/namnul.txt | wc -l) ]
then
	echo "Warning: duplicated namelists exist:" >&2
	diff -bBw namnul.txt $tmpdir
	exit 1
fi

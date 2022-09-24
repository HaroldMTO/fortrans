#!/bin/sh

usage()
{
	printf "
Description:
	Produce 'empty' concatenated Fortran namelist from Fortran 90 files, \
supposed to contain namelist declarations.

Synopsis:
	$(basename $0) SRC [-ext EXT] [-nofile] [-v] [-h]

Arguments:
	SRC: path from where to (recursively) search for Fortran files containing namelists \
(default: .)
	EXT : file extension(s) (see Details) targetting file type(s) to search namelists for
	-nofile: impede individual files search, only dirs search is performed
	-v: activate verbose mode
	-h: displays help and terminates normally

	Files produced are:
		- namnul.txt: empty concatenated ordered list of namelists
		- namelists.f90: list of all found namelist declarations, as a Fortran \
file. Note: namelist names and variables are sorted out.

Details:
	Extension(s) for file search is considered as \*.[EXT] (pattern in find).
	EXT is a list of colon (':') separated file extensions. Default is 'F90:f90:f'.
	Files are searched following extension(s) in EXT recursively from path SRC (find).
	Files targetted by EXT (default or user supplied) are assumed to all be Fortran code \
files.
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

if echo " $*" | grep -qE ' \-\w*h'
then
	usage
	exit
fi

src=""
ext="F90"
files=1
verbose=0
keep=0

while [ $# -ne 0 ]
do
	case $1 in
		-ext)
			ext=$2
			shift
			;;
		-nofile)
			files=0
			;;
		-v)
			verbose=1
			;;
		-keep)
			keep=1
			;;
		*)
			if [ -z "$src" ] && echo $1 | grep -qE '^[[:alnum:]/]'
			then
				(cd $1 >/dev/null) || exit 1
				src=$1
			else
				echo "$1 : unknown option, ignored" >&2
			fi
			;;
	esac

	shift
done

if echo "$ext" | grep -qE "\s+."
then
	echo "Error: spaces found in file extension (invalid)" >&2
	exit 1
fi

[ -z "$src" ] && src=.

set -e

#rm -f namelists.f90 namnul.txt

tmpdir=$(mktemp -d tmpXXX)
trap '[ $keep -eq 0 ] && rm -r $tmpdir' EXIT

echo "tmpdir is '$tmpdir'"

if [ $files -eq 1 ]
then
	echo "Looking for files in '$src' (file mode)"
	grep -iErl '^\s*namelist */' $src > $tmpdir/namfiles.lst || true

	[ $verbose -eq 1 ] &&
		echo "$(wc -l $tmpdir/namfiles.lst | awk '{print $1}') files match"
fi

if [ -n "$ext" ]
then
	echo "Looking for files with extensions '$ext' in '$src' (dir mode)"
	# list files, but write their dir
	# mindepth avoids current dir '.', as sed would make it a blank pattern for grep
	for e in $(echo $ext | tr ':' '\n' | xargs)
	do
		find $src -type f -name \*.$e -printf "%h/\n" | sed -re 's:^\./::'
	done | sort -u | grep -vE '^ *$' > $tmpdir/namdir.lst || true

	[ $verbose -eq 1 ] && echo "$(wc -l $tmpdir/namdir.lst | awk '{print $1}') dirs match"

	if [ -s $tmpdir/namfiles.lst -a -s $tmpdir/namdir.lst ]
	then
		[ $verbose -eq 1 ] && echo "Filter out dirs: $(cat $tmpdir/namdir.lst | xargs)"
		grep -vf $tmpdir/namdir.lst $tmpdir/namfiles.lst > $tmpdir/namfiles.tmp || true
		mv $tmpdir/namfiles.tmp $tmpdir/namfiles.lst
	fi

	if [ -s direxclude ]
	then
		grep -vf direxclude $tmpdir/namdir.lst  > $tmpdir/namdir.tmp
		mv $tmpdir/namdir.tmp $tmpdir/namdir.lst
	fi

	if [ -s $tmpdir/namdir.lst ]
	then
		rext=$(echo $ext | sed -re 's/(^|:)\.//g' -e 's:([^\])\.:\1\\.:g')
		echo "Rewrite $(wc -l $tmpdir/namdir.lst | awk '{print $1}') Fortran dirs"
		while read dd
		do
			[ $verbose -eq 1 ] && echo ". rewrite '$dd'"
			mkdir -p $tmpdir/$dd
			rewrite.sh -i $dd -o $tmpdir/$dd -ext "$rext" -tabs 0 >> $tmpdir/rewrite.log

			# only 'namelist' (not guaranteed...)
			for e in $(echo $ext | tr ':' '\n' | xargs)
			do
				ls -1 $tmpdir/$dd/ | grep -qE "\w\.$e$" || continue

				grep -ihE '^\s*namelist\s*/\s*\w+\s*/' $tmpdir/$dd/*.$e | \
					sed -re 's: *::g' -e 's:!.*::'
			done >> $tmpdir/nam.f90
		done < $tmpdir/namdir.lst
	fi
fi

if [ -s $tmpdir/namfiles.lst ]
then
	echo "Rewrite $(wc -l $tmpdir/namfiles.lst | awk '{print $1}') Fortran files"
	mkdir -p $tmpdir/namfiles
	while read ff
	do
		[ $verbose -eq 1 ] && echo ". rewrite $ff"
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

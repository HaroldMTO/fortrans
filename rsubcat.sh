#!/bin/sh

usage()
{
	printf "
Description :
	Concaténer récursivement les routines Fortran appelées dans les unités \
de programme Fortran (program ou subroutine) d'un fichier.

Syntaxe :
	rsubcat -i FICIN [-o FICOUT] [-h]

Arguments :
	-i : FICIN est le fichier Fortran contenant des déclarations de programmes \
ou subroutines
	-o : la sortie standard est redirigée vers le fichier FICOUT
	-h : affiche cette aide et termine le programme

Détails :
	Les unités de programme Fortran sont scannées les unes après les autres, \
puis affichées à l'écran. Les éventuels appels à diverses subroutines \
sont recherchés dans les unités trouvées, puis leur code source est \
recherché et affiché à la suite, le cas échéant.

	Une ligne blanche sépare 2 unités de programme.

Code retour :
	0 en cas de succès
	1 en cas d'échec

Dépendances :
	aucune

"
}

rsubcat()
{
	local sub subs subs1 fic

	if [ $# -ne 1 ]
	then
		echo "usage: rsubcat file" >&2
		return 1
	fi

	echo "! from $1:"
	cat $1
	echo ""

	subs1=$(grep -iE '^\s*subroutine +\w+\>' $1 | \
		sed -re 's:^\s*subroutine +(\w+).+:\1:i' | sort -u)
	echo "$subs1" >> $tmp

	subs=$(grep -iE '^\s*(if *\(.+\) *)?call +(\w+)' $1 | \
		sed -re 's:^\s*(if *\(.+\) *)?call +(\w+).+:\2:i' | sort -u | \
		grep -vf $tmp)
	echo "$subs" >> $tmp

	# problème : on scanne toutes les routines du fichier, pas seulement $sub
	for sub in $subs
	do
		for fic in $(grep --include=\*.[fF] --include=\*.[fF]90 \
			-rliE "^\s*subroutine $sub\\>" | sort -u)
		do
			if [ $fic = $ficout ] || diff -q $fic $1 >/dev/null
			then
				continue
			fi

			rsubcat $fic
		done
	done
}

if [ $# -eq 0 ] || echo $* | grep -q ' -h'
then
	usage
	exit
fi

ficin=""
ficout=""

while [ $# -ne 0 ]
do
	case $1 in
		-i)
			ficin=$2
			shift
			;;
		-o)
			ficout=$2
			shift
			;;
		-h)
			;;
		*)
			echo "$1 : option non traitée; ignorée" >&2
			;;
	esac

	shift
done

if [ ! -f $ficin ]
then
	echo "Erreur : fichier absent">&2
	exit 1
fi

if [ "$ficout" ]
then
	touch $ficout
	exec 1>$ficout
fi

tmp=$(mktemp --tmpdir=/tmp)

rsubcat $ficin

unlink $tmp

#!/bin/sh

fortrans=~/util/fortrans

Usage()
{
	printf "
Description :
	R��crire, selon la norme HP, des fichiers Fortran 90.
	Par d�faut, les fichiers d'extension F, f90 et F90 sont trait�s car \
consid�r�s Fortran 90.

Syntaxe :
	$(basename $0) -i DIRIN -o DIROUT [-ext EXT] [-h]

Arguments :
	DIRIN : r�pertoire (ou fichier !) Fortran 90
	DIROUT : r�pertoire de sortie, peupl� en miroir de DIRIN
	EXT : extension de fichier Fortran
	-h : affiche cette aide et termine le programme

D�tails :
	DIRIN et DIROUT peuvent �tre des fichiers ou des r�pertoires, \
ind�pendamment l'un de l'autre.

	Si DIRIN est un r�pertoire, tous les fichiers Fortran reconnus sous \
DIRIN seront trait�s.
	Si DIRIN est un fichier, ce fichier sera trait�, quelle que soit \
l'extension.

	Si DIROUT est un r�pertoire, ce r�pertoire sera cr��, au besoin. Si \
DIRIN est un r�pertoire, DIROUT aura la m�me arborescence et si DIRIN est un \
fichier, le nouveau fichier sera produit dans DIROUT.
	Si DIROUT est un fichier, le ou les fichiers de DIRIN seront produits \
concat�n�s dans DIROUT.

	Si une extension est fournie, elle sera trait�e et remplace les formats \
Fortran par d�faut (alors non trait�s).

Retour :
	non nul en cas d'erreur
	0 sinon

D�pendances :
	- logiciel R

"
}

dirin=""
dirout=""
ext=""
help=0

if [ $# -eq 0 ]
then
	help=1
fi

while [ $# -ne 0 ]
do
	case $1 in
		-i)
			dirin=$2
			shift
			;;
		-o)
			dirout=$2
			shift
			;;
		-ext)
			ext=$(echo $2 | sed -re 's:^\.::' -e 's:([^\])\.:\1\\.:g')
			shift
			;;
		-h)
			help=1
			;;
		*)
			echo "$1 : option non trait�e; ignor�e" >&2
			;;
	esac

	shift
done

if [ $help -eq 1 ]
then
	Usage
	exit
elif [ -z "$dirin" -a -z "$dirout" ]
then
	printf "Erreur : arguments obligatoires manquants :
dirin : '$dirin'
dirout : '$dirout'
" >&2
	exit 1
elif [ ! -e $dirin ]
then
	echo "Erreur : '$dirin inexistant" >&2
	exit 1
elif echo "$ext" | grep -qE "\s+."
then
	echo "Erreur : espaces dans l'extension (non valide)" >&2
	exit 1
fi

set -e

dirin=$(echo $dirin | sed -re 's:/+$::')
dirout=$(echo $dirout | sed -re 's:/+$::')

if [ -f $dirin ]
then
	echo "Total $dirin : 1 fichier Fortran 90"
	R --slave -f $fortrans/rewrite.R --args ficin=$dirin ficout="$dirout"
elif [ -d $dirin ]
then
	tmp=$(mktemp --tmpdir=/tmp)
	tmpdd=$(mktemp --tmpdir=/tmp)

	if [ -n "$ext" ]
	then
		find $dirin -type f -name \*.$ext > $tmp
	else
		find $dirin -type f -name \*.[fF]90 > $tmp
		find $dirin -type f -name \*.F >> $tmp
		ext="(F|[fF]90)"
	fi

	echo "Total $dirin : $(wc -l $tmp | awk '{print $1}') fichier(s) Fortran 90"
	sed -re 's:(.+)/.+:\1:' $tmp | sort -u > $tmpdd

	while read ddin
	do
		ddout=$(echo $ddin | sed -re "s:$dirin:$dirout:")
		mkdir -p $ddout
		R --slave -f $fortrans/rewrite.R --args ficin="$ddin" ficout="$ddout" \
			ext=$ext
	done < $tmpdd

	unlink $tmp
	unlink $tmpdd
fi

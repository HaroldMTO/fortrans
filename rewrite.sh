#!/bin/sh


fortrans=~/util/fortrans

Usage()
{
	printf "
Description :
	Réécrire, selon la norme HP, des fichiers Fortran 90.
	Les fichiers d'extension F, f90 et F90 sont traités.

Syntaxe :
	$(basename $0) -i DIRIN -o DIROUT [-h]

Arguments :
	DIRIN : répertoire (ou fichier !) Fortran 90
	DIROUT : répertoire de sortie, peuplé en miroir de DIRIN
	-h : affiche cette aide et termine le programme

Détails :
	DIRIN et DIROUT peuvent être des fichiers ou des répertoires, \
indépendamment l'un de l'autre.

	Si DIRIN est un répertoire, tous les fichiers Fortran reconnus sous \
DIRIN seront traités.
	Si DIRIN est un fichier, ce fichier sera traité, quelle que soit \
l'extension.

	Si DIROUT est un répertoire, ce répertoire sera créé, au besoin. Si \
DIRIN est un répertoire, DIROUT aura la même arborescence et si DIRIN est un \
fichier, le nouveau fichier sera produit dans DIROUT.
	Si DIROUT est un fichier, le ou les fichiers de DIRIN seront produits \
concaténés dans DIROUT.

Retour :
	non nul en cas d'erreur
	0 sinon

Dépendances :
	- logiciel R

"
}

dirin=""
dirout=""
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
		-h)
			help=1
			;;
		*)
			echo "$1 : option non traitée; ignorée" >&2
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
fi

set -e

dirin=$(echo $dirin | sed -re 's:/+$::')
dirout=$(echo $dirout | sed -re 's:/+$::')

if [ -f $dirin ]
then
	echo $dirin > lst
else
	find $dirin -name '*.[fF]90' > lst
	find $dirin -name '*.F' >> lst
fi

echo "Total $dirin : $(wc -l lst | awk '{print $1}') fichier(s) Fortran 90"
sed -re 's:(.+)/.+:\1:' lst | sort -u > lstdd

while read ddin
do
	ddout=$(echo $ddin | sed -re "s:$dirin:$dirout:")
	[ ! -e $ddout ] && mkdir -p $ddout

	R --slave -f $fortrans/rewrite.R --args ficin=$ddin ficout="$ddout"
done < lstdd

rm lst lstdd

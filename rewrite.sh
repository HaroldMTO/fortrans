#!/bin/sh

fortrans=~/util/fortrans

Usage()
{
	printf "
Description :
	Réécrire un fichier Fortran 90 selon la norme HP

Syntaxe :
	$(basename $0) -i FICIN -o FICOUT [-h]

Arguments :
	FICIN : fichier Fortran 90
	FICOUT : fichier FICIN mis à la norme HP
	-h : affiche cette aide et termine le programme

Retour :
	non nul en cas d'erreur
	0 sinon

Dépendances :
	- logiciel R

"
}

ficin=""
ficout=""
help=0

if [ $# -eq 0 ]
then
	help=1
fi

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
elif [ -z "$ficin" -a -z "$ficout" ]
then
	printf "Erreur : arguments obligatoires manquants :
ficin : '$ficin'
ficout : '$ficout'
" >&2
	exit 1
fi

R --slave -f $fortrans/rewrite.R --args ficin=$ficin ficout=$ficout

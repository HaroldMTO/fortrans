#!/bin/sh

f90=~/util/f90

Usage()
{
	printf "
Description :
	Réécrire un fichier Fortran selon la norme HP

Syntaxe :
	$(basename $0) -i FICIN -o FICOUT [-F] [-h]

Arguments :
	FICIN : fichier Fortran
	FICOUT : fichier FICIN mis à la norme HP
	-F : convertir au format libre lorsque le fichier est au format fixe
	-h : affiche cette aide et termine le programme

Retour :
	non nul en cas d'erreur
	0 sinon

Dépendances :
	- logiciel R

" | iconv -f LATIN1 -t ${LANG##*\.}
}

ficin=""
ficout=""
fixe=0
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
		-F)
			fixe=1
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

set -e

tmp1=$(mktemp XXX.f90)
tmp2=$(mktemp XXX.f90)

iconv -f LATIN1 -t UTF8 $ficin > $tmp1

sed -rf $f90/tof90.ere $tmp1 > $tmp2
if [ $fixe -eq 1 ]
then
	sed -rf $f90/tofree.ere $tmp2 > $tmp1
else
	cp $tmp2 $tmp1
fi

R --slave -f $f90/rewrite.R --args ficin=$tmp1 ficout=$ficout

unlink $tmp1
unlink $tmp2

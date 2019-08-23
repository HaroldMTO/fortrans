#!/bin/sh

fortrans=~/util/fortrans

Usage()
{
	printf "
Description :
	Réécrire, selon la norme HP, des fichiers Fortran 90.
	Par défaut, les fichiers d'extension F, f90 et F90 sont traités car \
considérés Fortran 90.

Syntaxe :
	$(basename $0) -i DIRIN -o DIROUT [-file] [-ext EXT] [-tabs TABS] \
[-width WIDTH] [-doc|-algo] [-h]

Arguments :
	DIRIN : fichier ou répertoire de fichiers Fortran 90
	DIROUT : fichier ou répertoire de sortie, peuplé en miroir de DIRIN
	EXT : extension de fichier Fortran
	TABS : 'taille' des tabulations (3 par défaut)
	WIDTH : taille maximale des lignes Fortran en sortie (90 par défaut)
	-file : creer des fichiers plutot que des repertoires
	-doc : ne retenir des fichiers Fortran que les commentaires
	-algo : ne retenir des fichiers Fortran que l'algorithme
	-v : mode verbeux, affiche le nom de chaque fichier traitÃ©
	-h : affiche cette aide et termine le programme

Détails :
	DIRIN et DIROUT peuvent être des fichiers ou des répertoires, \
indépendamment l'un de l'autre.

	Si DIRIN est un répertoire, tous les fichiers Fortran reconnus sous \
DIRIN seront traités. Si DIRIN est un fichier, ce fichier sera traité, quelle \
que soit l'extension.

	Si DIROUT est un répertoire existant, il aura le même contenu que DIRIN, \
qu'il soit fichier ou répertoire. Sinon, DIROUT sera un fichier et contiendra \
le ou les fichiers de DIRIN, produits et éventuellement concaténés dans DIROUT.

	Si une extension est fournie, elle sera traitée et remplace les formats \
Fortran par défaut (alors non traités).

	Par défaut, les lignes Fortran sont réalignées par des tabulations de \
taille 3 et redimensionnées à 90 caractères. Une taille de tabulation \
inférieure à 3 annule le réalignement et le redimensionnement des lignes. Une \
taille de ligne inférieure à 10 annule le redimensionnement des lignes.

	Les options '-doc' et '-call' s'excluent mutuellement.

Retour :
	non nul en cas d'erreur
	0 sinon

Dépendances :
	- logiciel R

"
}

dirin=""
dirout=""
tree=1
ext=""
tabs=3
width=90
opt="rewrite"
verbose="FALSE"
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
		-file)
			tree=0
			;;
		-ext)
			ext=$(echo $2 | sed -re 's:^\.::' -e 's:([^\])\.:\1\\.:g')
			shift
			;;
		-width)
			width=$2
			shift
			;;
		-tabs)
			tabs=$2
			shift
			;;
		-doc)
			opt="doc"
			;;
		-algo)
			opt="algo"
			;;
		-v)
			verbose="TRUE"
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
	echo "Total $dirin : 1 fichier Fortran 90 Ã  traiter"
	[ ! -e $dirout -a $tree -eq 1 ] && mkdir -p $dirout
	R --slave -f $fortrans/rewrite.R --args ficin=$dirin ficout="$dirout" \
		ext=$ext opt="$opt" width=$width tabs=$tabs verbose=$verbose
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

	echo "Total $dirin : $(wc -l $tmp | awk '{print $1}') fichiers Fortran 90 \
 Ã  traiter"
	sed -re 's:(.+)/.+:\1:' $tmp | sort -u > $tmpdd

	if [ ! -d $dirout ]
	then
		[ $tree -eq 1 -o $(wc -l $tmpdd | awk '{print $1}') -gt 1 ] &&
			mkdir -p $dirout || ddout=$dirout
	fi

	while read ddin
	do
		[ -d $dirout ] && ddout=$(echo $ddin | sed -re "s:$dirin:$dirout:")
		[ $verbose = "TRUE" ] && echo ". traitement $ddin -> $ddout"
		[ ! -e $ddout -a $tree -eq 1 ] && mkdir -p $ddout
		R --slave -f $fortrans/rewrite.R --args ficin="$ddin" ficout="$ddout" \
			ext=$ext opt="$opt" width=$width tabs=$tabs verbose=$verbose
	done < $tmpdd

	unlink $tmp
	unlink $tmpdd
fi

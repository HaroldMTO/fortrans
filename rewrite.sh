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
[-width WIDTH] [-nolower] [-force] [-doc|-algo|-intfb FUN] [-h]

Arguments :
	DIRIN : fichier ou répertoire de fichiers Fortran 90
	DIROUT : fichier ou répertoire de sortie, peuplé en miroir de DIRIN
	EXT : liste d'extensions de fichier Fortran (séparées par ':')
	TABS : 'taille' de l'indentation (3 par défaut)
	WIDTH : taille maximale des lignes Fortran en sortie (90 par défaut)
	-nolower : ne pas minisculiser le texte Fortran
	-file : créer des fichiers plutot que des répertoires
	-force : ne pas convertir les avertissements en erreur
	-doc : ne retenir des fichiers Fortran que les commentaires
	-algo : ne retenir des fichiers Fortran que l'algorithme
	-intfb: créer une interface Fortran pour la procédure FUN
	-v : mode verbeux, affiche le nom de chaque fichier traitÃ©
	-h : affiche cette aide et termine le programme

Détails :
	DIRIN et DIROUT peuvent être des fichiers ou des répertoires, \
indépendamment l'un de l'autre.

	Si DIRIN est un répertoire, tous les fichiers Fortran reconnus sous \
DIRIN seront traités. Si DIRIN est un fichier, ce fichier sera traité, quelle \
que soit son extension.

	Si DIROUT est un répertoire existant, il aura le même contenu que DIRIN, \
qu'il soit fichier ou répertoire. Sinon, DIROUT sera un fichier et contiendra \
le ou les fichiers de DIRIN, produits et éventuellement concaténés dans DIROUT.

	Si une liste d'extensions est fournie, elle sera traitée et remplace les \
formats Fortran par défaut (alors non traités).

	Par défaut, les lignes Fortran sont réalignées par des tabulations de \
taille 3 et redimensionnées à 90 caractères. Une taille de tabulation \
égale à 0 annule le réalignement et le redimensionnement des lignes. Une \
taille de ligne inférieure à 10 annule le redimensionnement des lignes.

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
lower="TRUE"
opt="rewrite"
verbose="FALSE"
force="FALSE"
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
			ext=$(echo $2 | sed -re 's/(^|:)\./\1/g')
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
		-intfb)
			opt="intfb"
			fun=$2
			shift
			;;
		-nolower)
			lower="FALSE"
			;;
		-force)
			force="TRUE"
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
elif [ $opt = "intfb" -a -z "$fun" ]
then
	echo "Erreur : procédure non fournie pour option intfb" >&2
	exit 1
fi

set -e

type R >/dev/null 2>&1 || module load intel R >/dev/null 2>&1

dirin=$(echo $dirin | sed -re 's:/+$::')
dirout=$(echo $dirout | sed -re 's:/+$::')

if [ -f $dirin ]
then
	[ $verbose = "TRUE" ] && echo "Fichier $dirin Fortran 90 Ã  traiter"
	R --slave -f $fortrans/rewrite.R --args ficin=$dirin ficout="$dirout" \
		opt="$opt" width=$width tabs=$tabs lower=$lower verbose=$verbose \
		force=$force fun=$fun
elif [ -d $dirin ]
then
	tmp=$(mktemp --tmpdir=/tmp)
	tmpdd=$(mktemp --tmpdir=/tmp)

	if [ -n "$ext" ]
	then
		for e in $(echo $ext | tr ':' ' ')
		do
			find $dirin -type f -name \*.$e
		done > $tmp

		# change '.' to '\.' (glob to REGEXP)
		ext=$(echo $ext | sed -re 's:([^\])\.:\1\\.:g')
	else
		find $dirin -type f -name \*.[fF]90 > $tmp
		find $dirin -type f -name \*.F >> $tmp
		ext="(F|[fF]90)"
	fi

	echo "Total $dirin : $(wc -l $tmp | awk '{print $1}') fichiers Fortran 90 \
 Ã  traiter, extensions '$ext'"
	sed -re 's:(.+)/.+:\1:' $tmp | sort -u > $tmpdd

	if [ ! -d $dirout ]
	then
		if [ $tree -eq 1 ]
		then
			mkdir -p $dirout
		else
			ddout=$dirout
			[ -s $ddout ] && rm $ddout
		fi
	fi

	while read ddin
	do
		[ -d $dirout ] && ddout=$(echo $ddin | sed -re "s:$dirin:$dirout:")
		[ $verbose = "TRUE" ] && echo ". traitement $ddin -> $ddout"
		[ ! -e $ddout -a $tree -eq 1 ] && mkdir -p $ddout
		R --slave -f $fortrans/rewrite.R --args ficin="$ddin" ficout="$ddout" \
			ext=$ext opt="$opt" width=$width tabs=$tabs lower=$lower \
			verbose=$verbose force=$force fun=$fun
	done < $tmpdd

	unlink $tmp
	unlink $tmpdd
fi

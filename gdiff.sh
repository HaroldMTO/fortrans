#!/bin/sh

set -e

usage()
{
	printf "
Description :
	applique 'git diff' sur path1 path2... (répertoires uniquement) et écrit le \
résultat dans des fichiers individuels

Syntaxe :
	$(basename $0) path1 path2... [-d] [--d] [-o diff-opt] [-h]

Arguments :
	-o: ajout les options listées dans diff-opt à la commande 'git diff'
	--d: supprime les options au format long ('--xxx') utilisées par défaut dans git diff
	-d: supprime les options au format court ('-xxx') utilisées par défaut dans git diff
	-h: affiche cette aide et termine normalement

Détails:
	Les options par défaut pour git diff sont '--stat=90 --summary --ignore-blank-lines \
--no-prefix -U0 -b -w'. Toute option ajoutée dans diff-opt modifie ce défaut. \
Les arguments -d and --d suppriment les options par défaut de git diff, format long et \
court respectivement.
	Les fichiers résultat sont nommés 'path.txt', sauf si path est '.' ou '..'. Dans \
ces 2 cas, les fichiers sont _.txt et _..txt.

Notes :
	Si path1 est du style 'a/b/c', le résultat c.txt sera dans a/b.
	Les éléments path1 path2... sont relatifs, comme le fait git

Retour :
	non 0 en cas d'erreur, 0 sinon
"
}

lst=""
ldef="--stat=90 --summary --ignore-blank-lines --no-prefix"
sdef="-U0 -b -w"

while [ $# -ne 0 ]
do
	case $1 in
	-h)
		usage
		exit
		;;
	-o)
		opt=$2
		shift
		;;
	--d)
		ldef=""
		;;
	-d)
		sdef=""
		;;
	*)
		lst="$lst $1"
	esac

	shift
done

for obj in $lst
do
	[ -d $obj ]

	if [ $obj = "." ]
	then
		fic=_.txt
	elif [ $obj = ".." ]
	then
		fic=_..txt
	else
		fic=$(echo $obj | sed -re 's:/$::').txt
	fi

	git diff $ldef $sdef $opt $obj > $fic
done

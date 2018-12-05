change = function(re)
{
	if (length(re) == 1) re[2] = ""
	re[2] = gsub("\\n","\n",re[2],fixed=TRUE)
	re[2] = gsub("\\t","\t",re[2],fixed=TRUE)

	re
}

reindent = function(texte)
{
	# blocin et blocout dépendent de comm, blocin dépend de blocout
	nul = "^$"
	comm = "^\s*!"
	ormots = "program|module|type|subroutine|function|if|do|select|where"
	blocout = sprintf("\<end *\<(%s)\>",ormots)
	blocin = sprintf("\<(%s)\>",ormots)
	alter = "\<(else(\>|if|where)|contains)"

	lignes = strsplit(texte,split="\n+")

	tab = 0

	for (i in seq(along=lignes)) {
		if (regexpr(nul,lignes[i]) > 0) {
			tabi = 0
		} else if (regexpr(comm,lignes[i]) > 0) {
			tabi = tab
		} else if (regexpr(blocout,lignes[i]) > 0) {
			tab = tab - 1
			tabi = tab
		} else if (regexpr(alter,lignes[i]) > 0) {
			tabi = tab
		} else if (regexpr(blocin,lignes[i]) > 0) {
			tabi = tab
			tab = tab + 1
		} else {
			tabi = tab -1
		}

		lignes[i] = paste(rep("\t",tabi),lignes[i],sep="")
	}

	paste(lignes,collapse="\n")
}

#lre = strsplit(readLines("~/util/f90/re_tab.txt"),":")
lre = strsplit(readLines("~/util/f90/re_to90.txt"),":")
lre = lapply(lre,change)

cat("Lecture des arguments\n")
args = strsplit(commandArgs(trailingOnly=TRUE),split="=")
cargs = lapply(args,function(x) strsplit(x[-1],split=":")[[1]])
names(cargs) = sapply(args,function(x) x[1])

flines = readLines(cargs$ficin)
stopifnot(all(regexpr("\n",flines) < 0))

fcode = paste(flines,collapse="\n")

for (re in lre) {
	fcode = gsub(re[1],re[2],fcode,useBytes=TRUE)
}

fcode = reindent(fcode)

writeLines(fcode,cargs$ficout)

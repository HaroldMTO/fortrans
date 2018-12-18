fortrans = "~/util/fortrans"

change = function(re)
{
	re[1] = gsub("\\[([^\\])*\\\\n","[\\1\n",re[1])

	if (length(re) == 1) re[2] = ""
	re[2] = gsub("\\n","\n",re[2],fixed=TRUE)
	re[2] = gsub("\\t","\t",re[2],fixed=TRUE)

	re
}

reindent = function(lignes)
{
	# blocin et blocout dépendent de comm, blocin dépend de blocout
	nul = "^$"
	comm = "^ *!"
	blocass = "^ *\\<end *associate\\>"
	blocout = "^ *\\<end(if|where|do)?\\>"
	mots = "\\<(program|module|subroutine|function|do|select)\\>"
	blocif = "\\<if\\>.+\\<then\\>"
	blocw = "^ *where\\>"
	blocnw = "^ *where\\>.+[^=]=[^=]"
	bloct = "\\<type *[^(]"
	blocin = sprintf("^ *(%s)",paste(c(mots,blocif,bloct),collapse="|"))
	alter = "^ *(else|contains)\\>"

	out = file("err","w")
	tab = 0

	for (i in seq(along=lignes)) {
		if (regexpr(nul,lignes[i]) > 0) {
			next
		} else if (regexpr(comm,lignes[i]) > 0) {
			tabi = tab
		} else if (regexpr(blocass,lignes[i]) < 0 &&
			regexpr(blocout,lignes[i]) > 0) {
			if (tab > 0) tab = tab - 1
			tabi = tab
		} else if (regexpr(alter,lignes[i]) > 0) {
			if (tab == 0) {
				cat(i,":",lignes[i],"\n",file=out)
			} else {
				tabi = tab - 1
			}
		} else if (regexpr(blocin,lignes[i]) > 0 ||
			(regexpr(blocw,lignes) > 0 && regexpr(blocnw,lignes[i]) < 0)) {
			tabi = tab
			tab = tab + 1
		} else {
			tabi = tab
		}

		stopifnot(tabi >= 0 && abs(tab-tabi) <= 1)
		lignes[i] = gsub("^ *",paste(rep("\t",tabi),collapse=""),lignes[i])
	}

	close(out)
	lignes
}

lre = strsplit(readLines("%s/re_to90.txt",fortrans),":")
lre = lre[! sapply(lre,function(x) length(x) == 0 || regexpr("^#",x[1]) > 0)]
lre = lapply(lre,change)

if (interactive()) browser()

args = strsplit(commandArgs(trailingOnly=TRUE),split="=")
cargs = lapply(args,function(x) strsplit(x[-1],split=":")[[1]])
names(cargs) = sapply(args,function(x) x[1])

flines = readLines(cargs$ficin)
stopifnot(all(regexpr("\n",flines) < 0))

texte = paste(flines,collapse="\n")
texte2 = texte
for (re in lre) texte2 = gsub(re[1],re[2],texte2,useBytes=TRUE)

flines2 = strsplit(texte2,"\n")[[1]]
cat(". reindent",length(flines2),"lignes\n")
flines3 = reindent(flines2)

cat(". writeLines",length(flines3),"lignes\n")
texte3 = paste(flines3,collapse="\n")
writeLines(texte3,cargs$ficout)


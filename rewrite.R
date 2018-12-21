fortrans = "~/util/fortrans"

change = function(re)
{
	re[1] = gsub("\\[([^\\])*\\\\n","[\\1\n",re[1])

	if (length(re) == 1) re[2] = ""
	re[2] = gsub("\\n","\n",re[2],fixed=TRUE)
	re[2] = gsub("\\t","\t",re[2],fixed=TRUE)

	gsub(";",":",re,fixed=TRUE)
}

reindent = function(lignes)
{
	# blocin et blocout dépendent de comm, blocin dépend de blocout
	nul = "^$"
	inc = "^ *#[^#]+"
	comm = "^ *!"
	blocass = "^ *end +associate\\>"
	blocout = "^ *end\\>"
	mots = "\\<(program|module|subroutine|function)\\>"
	blocl = "(\\w+:)? *(do|where|select)\\>"
	blocif = "(\\w+:)? *if\\>[^!]+\\<then\\>"
	bloct = "type *[^(]"
	blocin = sprintf("^ *(%s|%s)",mots,paste(c(blocl,blocif,bloct),collapse="|"))
	blocnw = "^ *(\\w+:)? *where\\>[^!]+[^=]=[^=]"
	alter = "^ *(else|contains)\\>"

	tab = 0

	for (i in seq(along=lignes)) {
		if (regexpr(nul,lignes[i]) > 0) {
			next
		} else if (regexpr(inc,lignes[i]) > 0) {
			tabi = 0
		} else if (regexpr(comm,lignes[i]) > 0) {
			tabi = tab
		} else if (regexpr(blocass,lignes[i]) < 0 &&
			regexpr(blocout,lignes[i]) > 0) {
			if (tab > 0) tab = tab - 1
			tabi = tab
		} else if (regexpr(alter,lignes[i]) > 0) {
			if (tab > 0) tabi = tab - 1
		} else if (regexpr(blocin,lignes[i]) > 0 && regexpr(blocnw,lignes[i])<0) {
			tabi = tab
			tab = tab + 1
		} else {
			tabi = tab
		}

		lignes[i] = gsub("^ *",paste(rep("\t",tabi),collapse=""),lignes[i])
	}

	lignes
}

lre = strsplit(readLines(sprintf("%s/re_to90.txt",fortrans)),":")
lre = lre[! sapply(lre,function(x) length(x) == 0 || regexpr("^ *#",x[1]) > 0)]
lre = lapply(lre,change)

if (interactive()) browser()

args = strsplit(commandArgs(trailingOnly=TRUE),split="=")
cargs = lapply(args,function(x) strsplit(x[-1],split=":")[[1]])
names(cargs) = sapply(args,function(x) x[1])

# ficin : 1 ou plusieurs fichiers ou 1 repertoire
ficin = cargs$ficin
if (length(ficin) == 1 && file.info(ficin)$isdir)
	ficin = dir(ficin,pattern="\\.(F|[fF]90)$",full.names=TRUE)

# ficout : 1 ou plusieurs fichiers (=#ficin) ou 1 repertoire
ficout = cargs$ficout
if (length(ficout) == 1 && file.exists(ficout) && file.info(cargs$ficout)$isdir)
	ficout = gsub(cargs$ficin,cargs$ficout,ficin)

stopifnot(length(ficin) == length(ficout))

nin = nout = 0

for (i in seq(along=ficin)) {
	cat(". conversion",ficin[i],"->",ficout[i],"\n")
	flines = readLines(ficin[i])
	flines = try(tolower(flines),silent=TRUE)
	if (is(flines,"try-error")) {
		flines = readLines(ficin[i],encoding="latin1")
		flines = tolower(flines)
	}

	nin = nin + length(flines)

	texte = paste(flines,collapse="\n")
	texte2 = texte
	for (re in lre) texte2 = gsub(re[1],re[2],texte2,useBytes=TRUE)

	flines2 = strsplit(texte2,"\n")[[1]]
	flines3 = reindent(flines2)

	nout = nout + length(flines3)
	texte3 = paste(flines3,collapse="\n")
	writeLines(texte3,ficout[i])
}

cat("Lignes :",nout,"/",nin,"(=",round(nout/nin*100),"%)\n")

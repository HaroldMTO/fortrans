fortrans = "~/util/fortrans"

change = function(re)
{
	re[1] = gsub("\\[([^\\]*)\\\\n([^\\]*)\\]","[\\1\n\\2]",re[1])

	if (length(re) == 1) re[2] = ""
	re[2] = gsub("\\n","\n",re[2],fixed=TRUE)
	re[2] = gsub("\\t","\t",re[2],fixed=TRUE)

	gsub(";",":",re,fixed=TRUE)
}

stripbang = function(texte)
{
	tt = texte
	n = 0

	while (regexpr("(^|\n)([^'\"\n!&]+)('([^']+|'')*'|\"([^\"]+|\"\")*\")",tt) > 0) {
		pos = regexec("(^|\n)([^'\"\n!&]+)('([^']+|'')*'|\"([^\"]+|\"\")*\")",tt)[[1]]
		ind = pos[4] + attr(pos,"match.length")[4] - 1
		s = substring(tt,pos[4],ind)
		s1 = gsub("!","",gsub("&( *![^\n]*)*\n( *&)*","",s))

		tt = substring(texte,n+ind+1)

		if (n+pos[4] == 1) {
			texte = sub(s,s1,texte)
			n = nchar(s1)
			next
		}

		deb = substring(texte,1,n+pos[4]-1)
		texte = paste(deb,s1,tt,sep="")
		n = nchar(deb) + nchar(s1)
	}

	texte
}

splitLine = function(s,ntab=1)
{
	# critère d'arrêt de la récursion
	# les tab comptent pour 1 char, mais ils en font 3 de large (valeur fixe)
	nt = nchar(sub("^(\t*).+","\\1",s))
	if (nchar(s)+2*nt <= 80) return(s)

	split = ","
	l = strsplit(s,split)[[1]]
	ind = which(cumsum(nchar(l)+nchar(split))+2*nt < 80)
	if (length(ind) == 0) {
		split = "\\.or\\."
		l = strsplit(s,split)[[1]]
		ind = which(cumsum(nchar(l)+nchar(split))+2*nt < 80)
		if (length(ind) == 0) {
			split = "\\.and\\."
			l = strsplit(s,split)[[1]]
			ind = which(cumsum(nchar(l)+nchar(split))+2*nt < 80)
			if (length(ind) == 0) return(s)
		}
	}

	s1 = sprintf("%s%s&\n",paste(l[ind],collapse=split),split)
	s2 = sprintf("%s%s",paste(rep("\t",nt+ntab),collapse=""),
		paste(l[-ind],collapse=split))
	paste(s1,paste(splitLine(s2,ntab=0),sep=""),sep="")
}

resize = function(lignes)
{
	for (i in which(nchar(lignes) > 80)) lignes[i] = splitLine(lignes[i])

	lignes
}

reindent = function(lignes)
{
	# blocin et blocout dÃ©pendent de comm, blocin dÃ©pend de blocout
	nul = "^$"
	inc = "^ *(#[^#]+|[0-9]{1,5})"
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

squelette = function(lignes)
{
	sq = "^\\t*(end )?\\<(program|module|subroutine|function|call|select|case|do|if|where|else|contains)\\>"
	#ind = rep(which(regexpr(sq,lignes) > 0),each=3) + seq(-1,1)
	#ind = unique(sort(ind))
	#lignes[ind[ind %in% seq(along=lignes)]]
	lignes[regexpr(sq,lignes) > 0]
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

if ("ficout" %in% names(cargs)) {
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
		texte2 = stripbang(texte)
		for (re in lre) texte2 = gsub(re[1],re[2],texte2,useBytes=TRUE)

		flines2 = strsplit(texte2,"\n")[[1]]
		flines3 = reindent(flines2)
		flines4 = resize(flines3)

		nout = nout + length(flines4)
		texte3 = paste(flines4,collapse="\n")
		writeLines(texte3,ficout[i])
	}

	cat("Lignes :",nout,"/",nin,"(=",round(nout/nin*100),"%)\n")
} else {
	ficin = ficin[basename(ficin) != "call.f90"]
	ficsq = sprintf("%s/call.f90",dirname(ficin[1]))

	nin = 0

	fsq = character()
	for (i in seq(along=ficin)) {
		flines = readLines(ficin[i])
		flines = try(tolower(flines),silent=TRUE)
		if (is(flines,"try-error")) {
			flines = readLines(ficin[i],encoding="latin1")
			flines = tolower(flines)
		}

		nin = nin + length(flines)

		flines2 = squelette(flines)
		fsq = c(fsq,"",flines2)
	}

	cat("Squelette",ficsq,length(fsq),"/",nin,"\n")
	writeLines(fsq[-1],ficsq)
}

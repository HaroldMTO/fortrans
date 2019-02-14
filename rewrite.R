fortrans = "~/util/fortrans"

change = function(re)
{
	# remplace '\\n' par '\n' dans '[...]'
	re[1] = gsub("\\[([^\\]*)\\\\n([^\\]*)\\]","[\\1\n\\2]",re[1])

	if (length(re) == 1) re[2] = ""
	re[2] = gsub("\\n","\n",re[2],fixed=TRUE)
	re[2] = gsub("\\t","\t",re[2],fixed=TRUE)

	gsub(";",":",re,fixed=TRUE)
}

stripbang = function(texte)
{
	strcontRE = "&([\t ]*![^\n]*)*\n+([\t ]*&)*"
	stringRE = sprintf(
		"(^|\n)([^'\"\n!&]+)('([^'&]+|''|%s)*'|\"([^\"&]+|\"\"|%s)*\")",strcontRE,
		strcontRE)
	tt = texte
	n = 0

	while (regexpr(stringRE,tt) > 0) {
		pos = regexec(stringRE,tt)[[1]]
		ind = pos[4] + attr(pos,"match.length")[4] - 1
		s = substring(tt,pos[4],ind)

		# fusion lignes continuées et suppression des '!'
		s1 = gsub("!","",gsub(strcontRE,"",s))

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

	if (regexpr("\\<if\\> *\\(.+\\) *\\<then\\>",s) > 0) {
		splits = c(".or.",".and.",",")
		resplits = c("\\.or\\.","\\.and\\.",",")
	} else if (regexpr("\\<if\\> *\\(.+\\)",s) > 0) {
		s = sub("(\\<if\\> *\\(.+\\)) +(\\.+)",
			sprintf("\\1&\n%s\\2",paste(rep("\t",nt+ntab),collapse="")),s)
		return(s)
	} else if (regexpr("(\\<where\\> +\\(.+\\)) +([^=]+=)",s) > 0) {
		s = sub("(\\<where\\> +\\(.+\\)) +([^=]+=)",
			sprintf("\\1&\n%s\\2",paste(rep("\t",nt+ntab),collapse="")),s)
		return(s)
	} else if (regexpr("\\<(call \\w+|associate *\\()",s) > 0) {
		splits = c("),",",")
		resplits = c("\\),",",")
	} else if (regexpr(" = ",s) > 0) {
		splits = c("+","-")
		resplits = c("\\+","-")
	} else {
		return(s)
	}

	for (i in seq(along=splits)) {
		l = strsplit(s,resplits[i])[[1]]
		ind = which(cumsum(nchar(l)+nchar(splits[i]))+2*nt < 80)
		if (length(ind)) break
	}

	if (length(ind) == 0) return(s)

	s1 = sprintf("%s%s&\n",paste(l[ind],collapse=splits[i]),splits[i])
	s2 = sprintf("%s%s",paste(rep("\t",nt+ntab),collapse=""),
		paste(l[-ind],collapse=splits[i]))
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
	inc = "^ *#"
	tag = "^ *[0-9]{1,5}"
	comm = "^ *!"
	mproc = "^ *\\<module +procedure\\> +"
	blocass = "^ *end +associate\\>"
	blocout = "^ *end\\>"
	unit = "((pure|impure|elemental|recursive|abstract) +)*(program|module|subroutine|interface)\\>"
	ftn = "((pure|impure|elemental|recursive|integer|real|double precision|character|complex|logical|type|class)(\\*\\d+|\\([[:alnum:]_=]+\\))? +)*function\\>"
	bloc = "(\\d+ +|\\w+ *: *)*((do|select)\\>|if\\>[^!]+\\<then\\>|where\\> *\\(([^()]+|\\(([^()]+|\\(([^()]+|\\([^()]+\\))+\\))+\\))+\\) *$)"
	bloct = "type *[^(]"
	blocin = sprintf("^ *(%s|%s|%s|%s)",unit,ftn,bloc,bloct)
	alter = "^ *(else|contains)\\>"

	tab = 0

	for (i in seq(along=lignes)) {
		if (regexpr(nul,lignes[i]) > 0) {
			next
		} else if (regexpr(inc,lignes[i]) > 0) {
			tabi = 0
		} else if (regexpr(comm,lignes[i]) > 0 || regexpr(mproc,lignes[i]) > 0) {
			tabi = tab
		} else if (regexpr(blocass,lignes[i]) < 0 &&
			regexpr(blocout,lignes[i]) > 0) {
			if (tab == 0) warning("tab nul avant blocout :",lignes[i])
			if (tab > 0) tab = tab - 1
			tabi = tab
		} else if (regexpr(alter,lignes[i]) > 0) {
			if (tab == 0) warning("tab nul avant alter :",lignes[i])
			if (tab > 0) tabi = tab - 1
		} else if (regexpr(blocin,lignes[i]) > 0) {
			tabi = tab
			tab = tab + 1
		} else if (regexpr(tag,lignes[i]) > 0) {
			tabi = 0
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

# ficin doit exister
if (file.info(cargs$ficin)$isdir) {
	ficin = dir(cargs$ficin,pattern="\\.(F|[fF]90)$",full.names=TRUE)
} else {
	ficin = cargs$ficin
}

if ("ficout" %in% names(cargs)) {
	# ficout : par défaut, est un répertoire
	if (! file.exists(cargs$ficout)) dir.create(cargs$ficout,recursive=TRUE)

	if (file.info(cargs$ficout)$isdir) {
		ficout = paste(cargs$ficout,basename(ficin),sep="/")
	} else {
		ficout = rep(tempfile(fileext=".f90"),length(ficin))
	}

	stopifnot(length(ficin) == length(ficout))

	nin = nout = 0

	for (i in seq(along=ficin)) {
		cat(". conversion",ficin[i],"\n")
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

	if (! file.info(cargs$ficout)$isdir) {
		invisible(file.remove(cargs$ficout))
		invisible(file.append(cargs$ficout,ficout))
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

fortrans = "~/util/fortrans"

toUTF8 = function(x)
{
	xtmp = iconv(x,to="UTF8")
	if (any(is.na(xtmp))) xtmp = iconv(x,"LATIN1","UTF8")

	xtmp
}

change = function(re)
{
	if (regexpr("\\\\$",re[1]) > 0) {
		re[1] = gsub("\\\\$","",re[1])
		re[1] = paste(re[1:2],collapse=";")
		re = re[-2]
	}

	# remplace '\\n' par '\n' dans '[...]'
	re[1] = gsub("\\[([^\\]*)\\\\n([^\\]*)\\]","[\\1\n\\2]",re[1])

	if (length(re) == 2) {
		re[3] = "FALSE"
	} else if (length(re) == 1) {
		re = c(re,"","FALSE")
	}

	re[2] = gsub("\\n","\n",re[2],fixed=TRUE)
	re[2] = gsub("\\t","\t",re[2],fixed=TRUE)

	re
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

rename = function(texte)
{
	t = "(integer|real|double precision|character|complex|logical|type|class)"

	vars = read.table(sprintf("%s/rename.txt",fortrans),header=TRUE)
	decl = sprintf("\n *\\<%s\\>[^\n]*::[^\n]*\\<%s\\>",t,vars$old)
	asso = sprintf("\n *associate\\([^\n]*\\<%s=>",vars$old)
	used = sprintf("[^%%]\\<%s\\>",vars$used)
	occ = sprintf("([^%%])\\<%s\\>",vars$old)
	replace = sprintf("\\1%s",vars$new)

	for (i in seq(dim(vars)[1])) {
		if (regexpr(used[i],texte) > 0 || (regexpr(decl[i],texte) <= 0 &&
			regexpr(asso[i],texte) <= 0)) next

		texte = gsub(occ[i],replace[i],texte)
	}

	texte
}

splitLine = function(s,ntab=1)
{
	# critère d'arrêt de la récursion
	# les tab comptent pour 1 char, mais ils en font 3 de large (valeur fixe)
	nt = nchar(sub("^(\\t*).+","\\1",s))
	if (nchar(s)+2*nt <= Gwidth) return(s)

	if (regexpr("\\<if\\> *\\(.+\\) *\\<then\\>",s) > 0) {
		splits = c(".or.",".and.",",")
		resplits = c("\\.or\\.","\\.and\\.",",")
	} else if (regexpr("\\<if\\> *\\(.+\\)",s) > 0) {
		s = sub("(\\<if\\> *\\(.+\\)) +(\\.+)",
			sprintf("\\1&\n%s\\2",paste(rep("\\t",nt+ntab),collapse="")),s)
		return(s)
	} else if (regexpr("(\\<where\\> +\\(.+\\)) +([^=]+=[^=])",s) > 0) {
		s = sub("(\\<where\\> +\\(.+\\)) +([^=]+=[^=])",
			sprintf("\\1&\n%s\\2",paste(rep("\\t",nt+ntab),collapse="")),s)
		return(s)
	} else if (regexpr(" = ",s) > 0) {
		splits = c("+","-","*","/")
		resplits = c("\\+","-","\\*","/")
	} else if (regexpr(" :: ",s) > 0) {
		return(s)
	} else {
		splits = c("),",",")
		resplits = c("\\),",",")
	}

	for (i in seq(along=splits)) {
		l = strsplit(s,resplits[i])[[1]]
		ind = which(cumsum(nchar(l)+nchar(splits[i]))+2*nt < Gwidth)
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
	for (i in which(nchar(lignes) > Gwidth)) lignes[i] = splitLine(lignes[i])

	unlist(strsplit(paste(lignes,collapse="\n"),split="\n"))
}

reindent = function(lignes,file)
{
	# blocin et blocout dépendent de comm, blocin dépend de blocout
	nul = "^$"
	inc = "^ *#"
	tag = "^ *[0-9]{1,5}"
	comm = "^ *!"
	mproc = "^ *\\<module +procedure\\> +"
	blocass = "^ *end +associate\\>"
	blocout = "^ *end( +\\w+|!|$)"
	unit = "((pure|impure|elemental|recursive|abstract) +)*(program|(sub)?module|subroutine|interface)\\>"
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
			if (tab == 0) warning("tab nul avant blocout :",lignes[i]," ",file)
			if (tab > 0) tab = tab - 1
			tabi = tab
		} else if (regexpr(alter,lignes[i]) > 0) {
			if (tab == 0) warning("tab nul avant alter :",lignes[i]," ",file)
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

keys = "associate|byte|integer|real|logical|character|complex|double precision|type|class|namelist|common|equivalence|(block )?data"
fun = "abs|exp|log|a?cos|a?sin|a?tan|pow|min|max|mod|modulo|sum|count|real|n?int"
array = "([[:alnum:]%]+)\\(([^()]+(\\([^()]+\\))?)+[^()]*\\)(%\\w+)?"
arg = sprintf("(\\w+=)?(%s|\\d+|'[^']+')",array)
oper = sprintf("%s|\\d+(\\.(\\d+)?)?([ed][+-]?\\d+)?(_jp(rb|rd|im))?",array)

findargs = function(s)
{
	args = character()

	while (nchar(s) > 0) {
		arg1 = sub(arg,"\\3",s)
		if (arg1 == s) return(c(args,s))

		if (nchar(arg1) > 0) args = c(args,arg1)
		s = sub(sprintf("%s( *, *)?",arg),"",s)
	}

	args
}

findops = function(s)
{
	ops = character()

	while (nchar(s) > 0) {
		op1 = sub(oper,"\\1",s)
		if (op1 == s) return(c(ops,s))

		if (nchar(op1) > 0) {
			if (regexpr(sprintf("(%s)\\(",fun),op1) > 0)
				op1 = sub(sprintf("(%s)\\((%s)[^()]*\\)",fun,oper),"\\2",op1)
			if (nchar(op1) > 0) ops = c(ops,op1)
		}

		s = sub(sprintf("%s(\\*\\*\\d+)? *[+*/-]?(%s)?[()]*",oper,fun),"",s)
	}

	ops
}

ll = "^\\t*(if \\(.+\\) )?\\w+ *=.+([/=]=|<=?|>=?|\\.(and|or)\\.)"
testa = "(\\t*)(else )?if \\((.+)\\) then"
testb = "(\\t*)if \\((([^()]+|\\([^()]+\\))+.*?)\\) *(.+)"
alt = "(\\t*)else"
mem = "\\t*(de)?allocate\\(.+\\)"
call = "(\\t*call\\s+\\w+)\\((.+)\\)"
seta = sprintf("(\\t*)%s *= *(.+)",array)

algo = function(line)
{
	if (regexpr(mem,line) > 0 || regexpr(ll,line) > 0 ||
		regexpr("\\t*(contains|subroutine|(?!end).*\\bfunction)\\b",line,
		perl=TRUE) > 0) {
		return(line)
	}

	if (regexpr(sprintf("\\t*(%s)",keys),line) > 0) {
		line = ""
	} else if (regexpr(testa,line) > 0) {
		line = sub(testa,"\\1\\2\\3 ->",line)
	} else if (regexpr(testb,line) > 0) {
		l = sub(testb,"\\4",line)
		l = algo(l)
		line = paste(sub(testb,"\\1\\2 ->\n\\1",line),l,collapse="\t")
	} else if (regexpr(alt,line) > 0) {
		line = sub(alt,"\\1/",line)
	} else if (regexpr(call,line) > 0) {
		s = regmatches(line,regexec(call,line))[[1]]
		args = findargs(s[3])
		line = sprintf("%s(%s)",s[2],paste(args,collapse=","))
	} else if (regexpr(seta,line) > 0) {
		s = regmatches(line,regexec(seta,line))[[1]]
		ops = findops(s[length(s)])

		# attention : 2 et 3 doivent être contigüs
		line = sprintf("%s <- %s",paste(s[2:3],collapse=""),
			paste(ops,collapse=","))
	} else {
		line = ""
	}

	line
}

squelette = function(flines)
{
	sq = "^\\t*(end )?\\<(program|module|subroutine|function|call|select|case|do|if|where|else|contains)\\>"
	#ind = rep(which(regexpr(sq,flines) > 0),each=3) + seq(-1,1)
	#ind = unique(sort(ind))
	#lignes[ind[ind %in% seq(along=flines)]]
	flines[regexpr(sq,flines) > 0]
}

rewrite = function(flines,filename)
{
	flines = tolower(flines)

	texte = paste(flines,collapse="\n")
	texte = stripbang(texte)
	for (re in lre) texte = gsub(re[1],re[2],perl=re[3],texte,useBytes=TRUE)

	texte = rename(texte)
	flines = strsplit(texte,"\n")[[1]]
	if (Gtabs >= 3) {
		flines = reindent(flines,filename)
		if (Gwidth > 10) flines = resize(flines)
	}

	flines
}

getdoc = function(flines,filename)
{
	texte = paste(flines,collapse="\n")
	if (regexpr("(^|\n)\\s*(sub)?module|program\\s",texte,ignore.case=TRUE) > 0) {
		texte = gsub("(^|\n)\\s*(sub)?module|program\\s(.*)\n\\s*contains\\s.+",
			"\\3",texte,ignore.case=TRUE)
		flines = unlist(strsplit(texte,"\n+"))
		flines = grep("^!",flines,value=TRUE)
	} else if (regexpr("(^|\n)\\s*(subroutine|[^\n!]*function)\\s",texte,
		ignore.case=TRUE) > 0) {
		doc = gsub("(^|\n)\\s*(subroutine|[^\n!]*function)\\s(?!!\\*{3,5} \\*\\w+)((![^\n]+\n+)+)",
			"\\1\\3",texte,ignore.case=TRUE,perl=TRUE)
		flines = unlist(strsplit(doc,"\n+"))
	} else {
		stop("contenu Fortran non reconnu")
	}

	flines = grep("!+((dir|dec|pgi)\\$|\\$|[ ~=*_+-]*$)",flines,
		ignore.case=TRUE,invert=TRUE,value=TRUE)
	gsub("^! *"," ",flines)
}

getalgo = function(flines,filename)
{
	flines = rewrite(flines)

	subin = "^\\t*(subroutine|(?!end).*function) "
	subout = "^\\t*end (subroutine|function)\\>"
	isin = isin2 = FALSE
	for (i in seq(along=flines)) {
		# isin: (isin or entering) and (isin2 or not outing)
		isin = (isin || regexpr(subin,flines[i],perl=TRUE) > 0) &&
			(isin2 || regexpr(subout,flines[i]) < 0)
		isin2 = isin && (isin2 || regexpr(subin,flines[i],perl=TRUE) > 0) &&
			(isin2 || regexpr(subout,flines[i]) < 0)

		if (! isin) {
			flines[i] = ""
			next
		}

		flines[i] = algo(flines[i])
	}

	strsplit(paste(flines,collapse="\n"),split="\n+")[[1]]
}

getcall = function(flines,filename)
{
	flines = rewrite(flines)
	squelette(flines)
}

re90 = readLines(sprintf("%s/re_to90.txt",fortrans))
lre = strsplit(re90,";")
lre = lre[! sapply(lre,function(x) length(x) == 0 || regexpr("^ *#",x[1]) > 0)]
lre = lapply(lre,change)

if (interactive()) browser()

args = strsplit(commandArgs(trailingOnly=TRUE),split="=")
cargs = lapply(args,function(x) unlist(strsplit(x[-1],split=":")))
names(cargs) = sapply(args,function(x) x[1])

if (cargs$opt == "doc") {
	action = getdoc
} else if (cargs$opt == "algo") {
	action = getalgo
} else if (cargs$opt == "call") {
	action = getcall
} else {
	action = rewrite
}

Gwidth = 90
if ("width" %in% names(cargs)) Gwidth = as.integer(cargs$width)

Gtabs = 3
if ("tabs" %in% names(cargs)) Gtabs = as.integer(cargs$tabs)

if (file.info(cargs$ficin)$isdir) {
	if (nchar(cargs$ext)) ext = sprintf("\\.%s$",cargs$ext)
	ficin = dir(cargs$ficin,pattern=ext,full.names=TRUE)
} else {
	ficin = cargs$ficin
}

ficout0 = NULL
if (file.exists(cargs$ficout) && file.info(cargs$ficout)$isdir) {
	ficout = paste(cargs$ficout,basename(ficin),sep="/")
} else if (length(ficin) > 1) {
	ficout = tempfile(fileext=rep(".f90",length(ficin)))
	ficout0 = cargs$ficout
} else {
	ficout = cargs$ficout
}

stopifnot(length(ficin) == length(ficout))

nin = nout = 0

for (i in seq(along=ficin)) {
	flines = readLines(ficin[i])
	flines = toUTF8(flines)

	nin = nin + length(flines)

	flines = action(flines,ficin[i])

	nout = nout + length(flines)
	texte = paste(flines,collapse="\n")
	writeLines(texte,ficout[i])
}

if (! is.null(ficout0)) {
	for (i in seq(along=ficout))
		cat("! from",ficin[i],"\n\n",file=ficout[i],append=TRUE)
	if (file.exists(ficout0)) file.remove(ficout0)
	invisible(file.append(ficout0,ficout))
}

cat("Lignes",cargs$ficin,":",nout,"/",nin,"(=",round(nout/nin*100),"%)\n")

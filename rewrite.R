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

regstop = function(x) x[1] + attr(x,"match.length")[1] - 1

strcont = "& *(!.*?)?\n+ *&?"
string = sprintf(
	"(^|\n)([^'\"\n!&]+)('([^'&]+|''|%s)*'|\"([^\"&]+|\"\"|%s)*\")",strcont,
	strcont)

stripbang = function(texte)
{
	# texte est mangé, tt est rempli par le début de texte modifié
	tt = ""

	repeat {
		ire = regexec(string,texte)
		if (ire[[1]][1] < 0) break

		s = regmatches(texte,ire)[[1]]

		# chaines : fusion lignes continuées et suppression des '!'
		# fait dans s[1] et non dans s[4] car action OK sur s[1]
		s1 = gsub("!","",gsub(strcont,"",s[1]))

		deb = substr(texte,1,regstop(ire[[1]]))
		deb = sub(s[1],s1,deb,fixed=TRUE)
		tt = paste(tt,deb,sep="")
		texte = substring(texte,regstop(ire[[1]])+1)
	}

	paste(tt,texte,sep="")
}

# indices de boucles scalaires uniquement, pas d'étiquette
arre = "\\w+(%\\w+|\\(([^()]+|\\(([^()]+|\\([^()]+\\))+\\))+\\))*"
indo = sprintf("(\n *)do (\\w+)=(\\w+),(\\w+)\n(%s =[^\n]+)\n *end do.*?\n",
	arre)

deloop = function(texte)
{
	# texte est mangé, tt est rempli par le début de texte modifié
	tt = ""
	has.loops = FALSE

	repeat {
		ire = regexec(indo,texte)
		if (ire[[1]][1] < 0) break

		s = regmatches(texte,ire)[[1]]

		# unsafe unlooping: out of array or not inner loop
		# ex: i+ or +i or ,i or z(a(i or f(z(i
		if (regexpr(sprintf("(,|\\w+\\()\\<%s\\>",s[3]),s[6]) > 0 ||
			regexpr(sprintf("[^(,]\\<%s\\>[^),]",s[3]),s[6]) > 0) {
			tt = paste(tt,substr(texte,1,regstop(ire[[1]])),sep="")
			texte = substring(texte,regstop(ire[[1]])+1)
			next
		}

		has.loops = TRUE

		# s[1] devient s[6] modifié
		s1 = gsub(sprintf("\\<%s\\>",s[3]),paste(s[4],s[5],sep=":"),s[6])
		s1 = sprintf("%s%s\n",s[2],s1)

		deb = substr(texte,1,regstop(ire[[1]]))
		deb = sub(s[1],s1,deb,fixed=TRUE)
		tt = paste(tt,deb,sep="")
		texte = substring(texte,regstop(ire[[1]])+1)
	}

	if (has.loops) tt = deloop(tt)

	paste(tt,texte,sep="")
}

# attention : 1 seule ligne, pas test vide ou ligne multi-instructions (;)
test = "\n( *if \\([^\n]+\\)) *then\n+(?! *(else|if *\\(|end *if)) *([^\n;]+)\n+ *end if\n"

deif = function(texte)
{
	# texte est mangé, tt est rempli par le début de texte modifié
	tt = ""

	repeat {
		ire = regexec(test,texte,perl=TRUE)
		if (ire[[1]][1] < 0) break

		s = regmatches(texte,ire)[[1]]

		s1 = sprintf("\n%s %s\n",s[2],s[4])

		deb = substr(texte,1,regstop(ire[[1]]))
		deb = sub(s[1],s1,deb,fixed=TRUE)
		tt = paste(tt,deb,sep="")
		texte = substring(texte,regstop(ire[[1]])+1)
	}

	paste(tt,texte,sep="")
}

types = "(integer|real|double precision|character|complex|logical|type|class)"

rename = function(texte,used,decl,asso,occ,replace)
{
	for (i in seq(along=used)) {
		if (regexpr(used[i],texte) > 0 || (regexpr(decl[i],texte) < 0 &&
			regexpr(asso[i],texte) < 0)) next

		texte = gsub(occ[i],replace[i],texte)
	}

	texte
}

stripasso = function(asso,texte)
{
	s = sub("^ *associate\\(([^\n]+)\\).*","\\1",asso)
	stopifnot(s != asso)

	lind = gregexpr(sprintf("\\w+=>%s",arre),s)
	pvars = sub("=>.+","",regmatches(s,lind)[[1]])

	for (p in pvars) {
		if (regexpr(sprintf("\\<%s\\>",p),texte) > 0) next

		# peut y avoir plusieurs occurrences du pointeur p
		asso = gsub(sprintf(",?\\<%s=>%s",p,arre),"",asso)
	}

	gsub("([(,]),+|,+([,)])","\\1\\2",asso)
}

cleanasso = function(flines)
{
	assoin = "^\\t*(associate *\\()"
	assoout = "^\\t*end associate\\>"

	ind1 = ind2 = integer()

	isin1 = isin2 = FALSE
	for (i in seq(along=flines)) {
		# isin1: (isin1 or entrant) and (isin2 or non sortant)
		# isin2: (isin2 or entrant) and non sortant
		isin2 = isin1 && (isin2 || regexpr(assoin,flines[i],perl=TRUE) > 0) &&
			regexpr(assoout,flines[i]) < 0
		isin1 = (isin1 || regexpr(assoin,flines[i],perl=TRUE) > 0) &&
			(isin2 || regexpr(assoout,flines[i]) < 0)

		# niveau 3 non traité (problème pour savoir quand on sort)
		if (isin2 && length(ind2) > 0 && regexpr(assoin,flines[i],perl=TRUE) > 0)
			break

		# associate imbriqués non disjoints, donc 1 et 2 ne s'excluent pas
		# on nettoie 2, puis 1
		if (isin2) {
			ind2 = c(ind2,i)
		} else if (length(ind2) > 0) {
			texte = paste(flines[ind2[-1]],collapse="\n")
			flines[ind2[1]] = stripasso(flines[ind2[1]],texte)
			ind2 = integer()
		}

		if (isin1) {
			ind1 = c(ind1,i)
		} else if (length(ind1) > 0) {
			texte = paste(flines[ind1[-1]],collapse="\n")
			flines[ind1[1]] = stripasso(flines[ind1[1]],texte)
			ind1 = integer()
		}
	}

	strsplit(paste(flines,collapse="\n"),split="\n+")[[1]]
}

reindent = function(lignes)
{
	# blocin et blocout dépendent de comm, blocin dépend de blocout
	nul = "^$"
	inc = "^ *#"
	tag = "^ *[0-9]{1,5}"
	comm = "^ *!"
	mproc = "^ *module +procedure\\> +"
	blocass = "^ *end +associate\\>"
	blocout = "^ *end( +\\w+|!|$)"
	unit = "((pure|impure|elemental|recursive|abstract) +)*(program|(sub)?module|subroutine|interface)\\>"
	ftn = sprintf("((pure|impure|elemental|recursive|%s)(\\*\\d+|\\([[:alnum:]_=]+\\))? +)*function\\>",types)
	bloc = "(\\d+ +|\\w+ *: *)*((do|select)\\>|if *\\([^!]+\\) *then\\>|where *\\(([^()]+|\\(([^()]+|\\(([^()]+|\\([^()]+\\))+\\))+\\))+\\) *$)"
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

splitLine = function(s,ntab=1)
{
	# après changement de caractère d'indentation (' ' -> '\t')
	stopifnot(regexpr("^\\t* +|\n",s) < 0)

	# critère d'arrêt de la récursion
	# les tab comptent pour 1 char, mais ils en font Gtabs de large
	nt = nchar(sub("^(\\t*).+","\\1",s))
	if (nchar(s)+(Gtabs-1)*nt <= Gwidth) return(s)

	if (regexpr("^\\t*if *\\(.+\\) *then\\>",s) > 0) {
		splits = c(".or.",".and.",",")
		resplits = c("\\.or\\.","\\.and\\.",",")
	} else if (regexpr("^\\t*if *\\(.+\\) *\\w+.+",s) > 0) {
		ms = regmatches(s,regexec("^(\\t*if *\\(.+\\)) *(\\w+.+)",s))[[1]]
		if (nchar(ms[3])+(nt+1)*(Gtabs-1) > Gwidth) {
			s2 = paste(paste(rep("\t",nt+1),collapse=""),ms[3],sep="")
			s = sprintf("%s then\n%s\n%send if",ms[2],splitLine(s2),
				paste(rep("\t",nt),collapse=""))
		} else {
			s = sprintf("%s&\n%s%s",ms[2],paste(rep("\t",nt+1),collapse=""),ms[3])
		}

		return(s)
	} else if (regexpr("^\\t*where *\\(.+\\) +[^=]+=[^=]",s) > 0) {
		s = sub("^(\\t*where +\\(.+\\)) +([^=]+=[^=])",
			sprintf("\\1&\n%s\\2",paste(rep("\t",nt+1),collapse="")),s)
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
		ind = which(cumsum(nchar(l)+nchar(splits[i]))+(Gtabs-1)*nt < Gwidth)
		if (length(ind)) break
	}

	if (length(ind) == 0) return(s)

	s1 = sprintf("%s%s&\n",paste(l[ind],collapse=splits[i]),splits[i])
	s2 = sprintf("%s%s",paste(rep("\t",nt+ntab),collapse=""),
		paste(l[-ind],collapse=splits[i]))

	s2 = splitLine(s2,0)

	paste(s1,s2,sep="")
}

resize = function(lignes)
{
	for (i in seq(along=lignes)) lignes[i] = splitLine(lignes[i])

	unlist(strsplit(paste(lignes,collapse="\n"),split="\n"))
}

#keys = sprintf("associate|%s|namelist|common|equivalence|(block )?data",types)
array = "([[:alnum:]%]+)(%\\w+|\\(([^()]+|\\(([^()]+|\\([^()]+\\))+\\))+\\))*"
fun1 = "abs|exp|log|a?cos|a?sin|a?tan|pow|min|max|mod|modulo|sum"
fun2 = "all|any|isnan|count|real|n?int|sign|huge|tiny|size|shape|trim"
fun = paste(fun1,fun2,sep="|")
arg = sprintf("%s|\\d+|'[^']+'",array)
num = "(\\d+(\\.\\d?)?|\\.\\d+)(_\\w+)?([ed][+-]?\\d+)?"
oper = sprintf("%s|%s",array,num)

findargs = function(s)
{
	args = character()

	# first get rid of arg sep, funs, nums and op seps
	s = sub(sprintf("(\\w+=|(%s)\\(|%s|[,()*/+-]+)*",fun,num),"",s)

	while (nchar(s) > 0) {
		arg1 = sub(arg,"\\3",s)
		if (arg1 == s) return(c(args,s))

		# get rid of potentially present intrinsic functions
		arg1 = sub(sprintf("(%s)\\(%s(,[^()]+)*\\)",fun,array),"\\2",arg1)
		if (nchar(arg1) > 0) args = c(args,arg1)

		s = sub(sprintf("%s(\\w+=|(%s)\\(|%s|[,()*/+-]+)*",arg,fun,num),"",s)
	}

	args
}

findops = function(s)
{
	# first get rid of op seps, funs and nums
	s = sub(sprintf("([ ()+*/-]+|(%s)\\(|%s)*",fun,num),"",s)

	ms = regmatches(s,gregexpr(array,s))[[1]]
	ops = sub(sprintf("(%s)\\(%s(,[^()]+)*\\)",fun,array),"\\2",
		grep("^[[:alpha:]]+",ms,value=TRUE))
	return(ops)

	ops = character()

	while (nchar(s) > 0) {
		op1 = sub(oper,"\\1",s)
		if (op1 == s) return(c(ops,s))

		# get rid of potentially present intrinsic functions
		op1 = sub(sprintf("(%s)\\(%s(,[^()]+)*\\)",fun,array),"\\2",op1)
		if (nchar(op1) > 0) ops = c(ops,op1)

		s = sub(sprintf("%s([ ()+*/-]+|(%s)\\(|%s)*",oper,fun,num),"",s)
	}

	ops
}

ll = "(\\t*)if \\((.+)\\) (\\w+ *=.+)"
testa = "(\\t*)(else )?if \\((.+)\\) then"
testb = "(\\t*)if \\((.+)\\) (\\w+.+)"
alt = "(\\t*)else$"
mem = "\\t*(de)?allocate\\(.+\\)"
call = "(\\t*call \\w+)\\((.+)\\)"
seta = sprintf("(\\t*)%s = *(\\(+|%s[*/+-]+)*(.+)",array,num)

algo = function(line)
{
	if (regexpr("^\\t*(contains|subroutine|(?!end).*\\bfunction)\\b",line,
		perl=TRUE) > 0) {
	} else if (regexpr("^\\t*(associate|(if \\(.+\\) )?(de)?allocate)\\(",line) > 0) {
		line = ""
		return(line)
	}

	if (regexpr(ll,line) > 0) {
		line = sub(ll,"\\1\\2 -> \\3",line)
	} else if (regexpr(testa,line) > 0) {
		line = sub(testa,"\\1\\2\\3 ->",line)
	} else if (regexpr(testb,line) > 0) {
		l = sub(testb,"\\3",line)
		l = algo(l)
		line = paste(sub(testb,"\\1\\2 ->\n\\1",line),l,sep="\t")
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

rewrite = function(flines)
{
	flines = tolower(flines)

	texte = paste(flines,collapse="\n")
	texte = stripbang(texte)
	for (re in lre) texte = gsub(re[1],re[2],perl=re[3],texte,useBytes=TRUE)
	stopifnot(regexpr("\\t| \n",texte) < 0)

	texte = deloop(texte)
	texte = deif(texte)
	texte = with(vars,rename(texte,used,decl,asso,occ,replace))

	flines = strsplit(texte,"\n")[[1]]
	flines = cleanasso(flines)

	if (Gtabs == 0) {
		for (i in seq(along=flines)) flines[i] = sub("^ *","",flines[i])
	} else if (Gtabs >= 3) {
		flines = reindent(flines)
		if (Gwidth > 10) flines = resize(flines)
	}

	stopifnot(any(regexpr("\n ",flines) < 0))
	flines
}

getdoc = function(flines)
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

getalgo = function(flines)
{
	flines = rewrite(flines)

	subin = "^\\t*(subroutine|(?!end).*function) "
	subout = "^\\t*end (subroutine|function)\\>"
	isin = isin2 = FALSE
	for (i in seq(along=flines)) {
		# isin2 (avant) : sin and (isin2 or entering) and not outing
		# isin : (isin or entering) and (isin2 or not outing)
		isin2 = isin && (isin2 || regexpr(subin,flines[i],perl=TRUE) > 0) &&
			regexpr(subout,flines[i]) < 0
		isin = (isin || regexpr(subin,flines[i],perl=TRUE) > 0) &&
			(isin2 || regexpr(subout,flines[i]) < 0)

		if (! isin) {
			flines[i] = ""
			next
		}

		flines[i] = algo(flines[i])
	}

	strsplit(paste(flines,collapse="\n"),split="\n+")[[1]]
}

getcall = function(flines)
{
	flines = rewrite(flines)
	squelette(flines)
}

re90 = readLines(sprintf("%s/re_to90.txt",fortrans))
lre = strsplit(re90,";")
lre = lre[! sapply(lre,function(x) length(x) == 0 || regexpr("^ *#",x[1]) > 0)]
lre = lapply(lre,change)

vars = read.table(sprintf("%s/rename.txt",fortrans),header=TRUE)
vars$used = sprintf("[^%%]\\<%s\\>",vars$used)
vars = cbind(vars,
	decl=sprintf("\n *\\<%s\\>[^\n]*::[^\n]*\\<%s\\>",types,vars$old),
	asso=sprintf("\n *associate\\([^\n]*\\<%s=>",vars$old),
	occ=sprintf("([^%%])\\<%s\\>",vars$old),replace=sprintf("\\1%s",vars$new))

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

	flines = action(flines)

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

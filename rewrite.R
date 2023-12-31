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

# pas besoin d'interdire les commentaires : on est hors chaine, commentaire ou continuation
nocomm = "(?![^'\"\n!]*!+[^\n]*)"
cont = "&( |\\t)*(![^\n]*)?\n\\s*&?"
string = sprintf("(^|\n)[^'\"\n!&]+('([^'&]+|''|%s)*'|\"([^\"&]+|\"\"|%s)*\")",cont,cont)

stripcont = function(texte)
{
	# texte est mangé, tt est rempli par le début de texte modifié
	tt = ""

	repeat {
		ire = regexec(string,texte)
		if (ire[[1]][1] < 0) break

		s = regmatches(texte,ire)[[1]]

		# remplacement provisoire ',' par ',;' (protection ',' en chaines)
		s3 = gsub(",",",;",s[3])

		# suppression des '...' (externes, chaine vide ou non), PUIS suppression
		# des '' internes (dans cet ordre)
		if (regexpr("\"",s3) < 0) {
			s3 = sub("^'(.*)'$","\"\\1\"",s3)
			s3 = gsub("''","'",s3)
		}

		# chaines : fusion lignes continuées, suppression des '!'
		s1 = sub(s[3],s3,s[1],fixed=TRUE)
		s1 = gsub(" *!+",".",gsub(cont,"",s1))

		deb = substr(texte,1,regstop(ire[[1]]))
		deb = sub(s[1],s1,deb,fixed=TRUE)

		# substr sur deb car avant ire[[1]][3], deb=texte
		if (ire[[1]][3] == 1) {
			deb0 = ""
		} else if (Glower) {
			deb0 = tolower(substr(deb,1,ire[[1]][3]-1))
		} else {
			deb0 = substr(deb,1,ire[[1]][3]-1)
		}

		deb1 = substring(deb,ire[[1]][3])
		tt = paste(tt,deb0,deb1,sep="")
		texte = substring(texte,regstop(ire[[1]])+1)
	}

	if (Glower) texte = tolower(texte)

	paste(tt,texte,sep="")
}

cont2 = "&( |\\t)*(![^\n]*)?\n(\\s+|![^\n]*\n)*(&( |\\t)*)?"
inst = sprintf("((^|\n)([^'\"\n!&]+|'([^']+|'')*'|\"([^\"]+|\"\")*\")+)%s",cont2)

mergecont = function(texte)
{
	n = 0
	repeat {
		ire = regexec(inst,texte)
		if (ire[[1]][1] < 0) break

		s = regmatches(texte,ire)[[1]]

		texte = sub(s[1],s[2],texte,fixed=TRUE)
		if (n == 1000) {
			warning("fusion lignes continuees arretee (> 1000 fusions)")
			return(texte)
		}

		n = n+1
	}

	texte
}

# indices de boucles scalaires uniquement, pas d'étiquette
arre = "[a-z]\\w*(%\\w+|\\(([^()]+|\\(([^()]+|\\([^()]+\\))+\\))+\\))*"
indo = sprintf("(\n *)do (\\w+)=(\\w+),(\\w+)\n( +%s =[^\n]+)\n *end do.*?\n",
	arre)

deloop = function(texte)
{
	# texte est mangé, tt est rempli par le début de texte modifié
	tt = ""
	has.loops = FALSE

	repeat {
		ire = regexec(indo,texte,ignore.case=TRUE)
		if (ire[[1]][1] < 0) break

		s = regmatches(texte,ire)[[1]]

		# unsafe unlooping: out of array or not inner loop
		# ex: i+ or +i or ,i or z(a(i or f(z(i or z(...,a(i
		if (regexpr(sprintf("(,|(,|\\w+\\()\\w+\\()\\<%s\\>",s[3]),s[6]) > 0 ||
			regexpr(sprintf("[^(]\\<%s\\>",s[3]),s[6]) > 0 ||
			regexpr(sprintf("\\(\\<%s\\>[^),]",s[3]),s[6]) > 0) {
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
		ire = regexec(test,texte,perl=TRUE,ignore.case=TRUE)
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

rename = function(texte,used,decl,asso,occ,replace)
{
	for (i in seq(along=used)) {
		if (regexpr(used[i],texte,ignore.case=TRUE) > 0 ||
			(regexpr(decl[i],texte,ignore.case=TRUE) < 0 &&
			regexpr(asso[i],texte,ignore.case=TRUE) < 0)) next

		texte = gsub(occ[i],replace[i],texte,ignore.case=TRUE)
	}

	texte
}

dums = sprintf("^ *%s\\>.*,intent\\(%s\\)",rep(c("integer","logical","real"),
	each=3),c("in","inout","out"))
store = c(",allocatable\\>",",pointer\\>")

orderstore = function(indg,flines)
{
	if (length(indg) == 0) return(indg)

	inds = unlist(lapply(store,grep,flines[indg],ignore.case=TRUE))
	if (length(inds) == 0) return(indg)

	c(indg[-inds],indg[inds])
}

ordergroup = function(ind,flines,groups)
{
	# ind est mangé, les éléments placés sont ajoutés dans indo
	indo = integer()

	while (length(ind) > 1) {
		# assure des groupes continus de déclarations (donc dans une même routine)
		if (all(diff(ind) == 1)) {
			ig = length(ind)
		} else {
			ig = min(which(diff(ind) > 1))
		}

		indg = ind[1:ig]
		lindg = lapply(groups,grep,flines[indg],ignore.case=TRUE)
		ii = unlist(lapply(lindg,orderstore,flines[indg]))
		if (length(ii) > 0) {
			indo = c(indo,c(indg[-ii],indg[ii]))
		} else {
			indo = c(indo,indg)
		}

		ind = ind[-(1:ig)]
	}

	# et le reste éventuel de ind
	c(indo,ind)
}

locali = sprintf("%s\\>(\\*\\d+\\>|\\(kind=\\w+\\))?",c("integer","real"))
locall = "logical\\>"
localc = "character\\>(\\*\\d+\\>|\\(len=(\\*|\\d+)\\))?"
localt = "(type|class)\\>\\(\\w+\\)"
locals = sprintf("^ *%s.*::",c(locali,locall,localc,localt))

orderlocal = function(flines)
{
	indi = grep(",intent\\((in|out|inout)\\).*::",flines,ignore.case=TRUE,invert=TRUE)

	re = paste(locals,collapse="|")
	indl = grep(re,flines[indi],ignore.case=TRUE)

	# suppression des lignes vides ou de commentaires
	i0 = indi[indl[which(diff(indi[indl]) == 2)]+1]
	if (any(regexpr("^ *! *((dec|dir|pgi)\\$|\\$omp|\\$acc|ocl) ",flines[i0],
		ignore.case=TRUE) > 0)) {
		return(flines)
	}

	i0 = i0[regexpr("^ *($|!)",flines[i0]) > 0]

	if (length(i0) > 0) {
		flines = flines[-i0]
		indi = grep(",intent\\((in|out|inout)\\).*::",flines,ignore.case=TRUE,invert=TRUE)
		indl = grep(re,flines[indi],ignore.case=TRUE)
	}

	indo = ordergroup(indl,flines[indi],locals)

	flines[indi[indl]] = flines[indi[indo]]

	flines
}

arrayd = "[a-z]\\w*(\\(([^()]+|\\([^()]+\\))+\\))"
blocks = "\\(\\w+%yrdim%nprom\\w+(,([^,]+)*),\\w+%yrdim%ngpblks\\)"

declarassume = function(flines)
{
	ind = grep(",intent\\((in|out|inout)\\).*::",flines,ignore.case=TRUE)

	indo = ordergroup(ind,flines,dums)
	flines[ind] = flines[indo]

	for (i in ind) {
		s = sub(".+:: *","",flines[i])
		ms = regmatches(s,gregexpr(arrayd,s,ignore.case=TRUE))[[1]]
		if (length(ms) == 0) next

		# remplace (nprom?,*,ngpblks) par (ngptot,*)
		#ms = gsub(blocks,"(ngptot\\1)",ms,ignore.case=TRUE)

		# conversion 'assumed-shape'
		ms = gsub("(\\(|,)([^,()]+|\\([^,()]+\\))+","\\1:",ms)
		flines[i] = sub(s,paste(ms,collapse=","),flines[i],fixed=TRUE)
	}

	flines
}

declarn = "(\n *(?:real|integer|logical|character)\\>[^\n]*::) *([^\n]+)(\\1 *([^\n]+))"

declarmerge = function(texte)
{
	# texte n'est pas mangé

	repeat {
		ire = regexec(declarn,texte,ignore.case=TRUE)
		if (ire[[1]][1] < 0) break

		s = regmatches(texte,ire)[[1]]
		s1 = sprintf("%s %s,%s",s[2],s[3],s[5])

		texte = sub(s[1],s1,texte,fixed=TRUE)
	}

	texte
}

stripblocks = function(flines)
{
	gsub("([a-z]\\w*)\\([^()]*,[ik]bl\\)","\\1",flines,ignore.case=TRUE)
}

stripasso = function(asso,texte)
{
	s = sub("^ *associate *\\(([^\n]*)\\).*","\\1",asso,ignore.case=TRUE)
	stopifnot(s != asso)
	if (regexpr("=>",s) < 0) return(asso)

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
	assoin = "^ *associate *\\("
	assoout = "^ *end associate\\>"
	assovide = "^ *associate *\\( *\\)"

	ind1 = ind2 = indsuppr = integer()

	wasin1 = isin1 = isin2 = FALSE
	for (i in seq(along=flines)) {
		# isin1: (isin1 or entrant) and (isin2 or non sortant)
		# isin2: (isin2 or entrant) and non sortant
		wasin1 = isin1
		isin1 = (isin1 || regexpr(assoin,flines[i],ignore.case=TRUE) > 0) &&
			(isin2 || regexpr(assoout,flines[i],ignore.case=TRUE) < 0)
		isin2 = wasin1 &&
			(isin2 || regexpr(assoin,flines[i],ignore.case=TRUE) > 0) &&
			regexpr(assoout,flines[i],ignore.case=TRUE) < 0

		# niveau 3 non traité (problème pour savoir quand on sort)
		if (isin2 && length(ind2) > 0 &&
			regexpr(assoin,flines[i],ignore.case=TRUE) > 0) {
			isin1 = FALSE
			isin2 = FALSE
			break
		}

		# associate imbriqués non disjoints, donc 1 et 2 ne s'excluent pas
		# on nettoie 2, puis 1
		if (isin2) {
			ind2 = c(ind2,i)
		} else if (length(ind2) > 0) {
			texte = paste(flines[ind2[-1]],collapse="\n")
			flines[ind2[1]] = stripasso(flines[ind2[1]],texte)
			if (regexpr(assovide,flines[ind2[1]],ignore.case=TRUE) > 0)
				indsuppr = c(indsuppr,ind2[1],ind2[length(ind2)]+1)

			ind2 = integer()
		}

		if (isin1) {
			ind1 = c(ind1,i)
		} else if (length(ind1) > 0) {
			texte = paste(flines[ind1[-1]],collapse="\n")
			flines[ind1[1]] = stripasso(flines[ind1[1]],texte)
			if (regexpr(assovide,flines[ind1[1]],ignore.case=TRUE) > 0)
				indsuppr = c(indsuppr,ind1[1],ind1[length(ind1)]+1)

			ind1 = integer()
		}
	}

	stopifnot(! isin1 && ! isin2)

	if (length(indsuppr) > 0) flines = flines[-indsuppr]
	flines
}

types = "integer|real|double precision|character|complex|logical|type|class"
subatt = "(im)?pure|elemental|recursive"
# blocin et blocout dépendent de comm, blocin dépend de blocout
nul = "^$"
inc = "^ *#"
tag = "^ *[0-9]{1,5}"
comm = "^ *!"
mproc = "^ *module +procedure\\> +"
blocass = "^ *end +associate\\>"
blocout = "^ *end( +\\w+| *!|$)"
unit = sprintf("program|(sub)?module|((%s) +)*(subroutine|(abstract +)?interface)\\b",subatt)
ftn = sprintf("((%s|(%s)(\\*\\d+|\\([[:alnum:]_=]+\\))?) +)*function +",subatt,types)
blocw = "where *\\(([^()]+|\\(([^()]+|\\(([^()]+|\\([^()]+\\))+\\))+\\))+\\) *$"
bloc = sprintf("(\\d+ +|\\w+ *: *)*((do|select)\\>|if *\\([^!]+\\) *then\\>|%s)",blocw)
blocin = sprintf("^ *(%s|%s|%s)",unit,ftn,bloc)
bloct = "^ *type\\b(?! +(is +\\(|default)| *\\()"
blocsel = "^ *((type|class) +(is *\\(|default\\>)|case( *\\(| +default\\>))"
alter = "^ *(else|contains)\\>"

reindent = function(lignes)
{
	tab = 0

	for (i in seq(along=lignes)) {
		if (regexpr(nul,lignes[i]) > 0) {
			next
		} else if (regexpr(inc,lignes[i]) > 0) {
			if (regexpr("#if(n?def)?\\>",lignes[i],ignore.case=TRUE) > 0) {
				tab0 = tab
			} else if (regexpr("#else",lignes[i],ignore.case=TRUE) > 0) {
				tab = tab0
			}

			tabi = 0
		} else if (regexpr(comm,lignes[i]) > 0 || regexpr(mproc,lignes[i],
			ignore.case=TRUE) > 0) {
			tabi = tab
		} else if (regexpr(blocass,lignes[i],ignore.case=TRUE) < 0 &&
			regexpr(blocout,lignes[i],ignore.case=TRUE) > 0) {
			if (tab == 0) warning("tab nul avant blocout :",lignes[i])
			if (tab > 0) tab = tab - 1
			tabi = tab
		} else if (regexpr(alter,lignes[i],ignore.case=TRUE) > 0) {
			if (tab == 0) warning("tab nul avant alter :",lignes[i])
			if (tab > 0) tabi = tab - 1
		} else if (regexpr(blocin,lignes[i],ignore.case=TRUE) > 0 ||
			regexpr(bloct,lignes[i],ignore.case=TRUE,perl=TRUE) > 0) {
			tabi = tab
			tab = tab + 1
		} else if (regexpr(blocsel,lignes[i],ignore.case=TRUE) > 0) {
			tabi = tab - 1
		} else if (regexpr(tag,lignes[i]) > 0) {
			tabi = 0
		} else {
			tabi = tab
		}

		lignes[i] = gsub("^ *",paste(rep("\t",tabi),collapse=""),lignes[i])
	}

	lignes
}

splitLine = function(s,ntab=1,call=FALSE,lassign=FALSE)
{
	# après changement de caractère d'indentation (' ' -> '\t')

	# critère d'arrêt de la récursion
	# les tab comptent pour 1 char, mais ils en font Gtabs de large
	nt = nchar(sub("^(\\t*).+","\\1",s))
	if (nchar(s)+(Gtabs-1)*nt <= Gwidth) return(s)

	if (regexpr("^\\t*!",s) > 0) {
		return(s)
	} else if (regexpr("^\\t*(if *\\()?.+\\) *then\\>",s,ignore.case=TRUE) > 0) {
		splits = c(".or.",".and.",",")
	} else if (regexpr("^\\t*if *\\(.+\\) *\\w+.+",s,ignore.case=TRUE) > 0) {
		ire = regexec("^(\\t*if *\\(.+\\)) *(\\w+.+)",s,ignore.case=TRUE)
		ms = regmatches(s,ire)[[1]]
		if (nchar(ms[2])+(Gtabs-1)*nt+5 > Gwidth ||
			nchar(ms[3])+(nt+1)*Gtabs > Gwidth) {
			s2 = sprintf("%s then",ms[2])
			s3 = paste(paste(rep("\t",nt+1),collapse=""),ms[3],sep="")
			s = sprintf("%s\n%s\n%send if",splitLine(s2),splitLine(s3),
				paste(rep("\t",nt),collapse=""))
		} else {
			s = sprintf("%s&\n%s%s",ms[2],paste(rep("\t",nt+1),collapse=""),ms[3])
		}

		return(s)
	} else if (regexpr("^\\t*where *\\(.+\\) +[^=]+=[^=]",s,
		ignore.case=TRUE) > 0) {
		s = sub("^(\\t*where +\\(.+\\)) +([^=]+=[^=])",
			sprintf("\\1&\n%s\\2",paste(rep("\t",nt+1),collapse="")),s,
			ignore.case=TRUE)
		return(s)
	} else if (call ||
		regexpr("^\\t*(call +\\w|print\\>|(open|read|write) *\\(.+\\)|\\w+.*::)",
		s,ignore.case=TRUE) > 0) {
		splits = ","
		call = TRUE
	} else if (lassign ||
		regexpr("^\\t*(.+ = )?(\\(+|\\.not\\.)*\\w+.*\\.(and|or)\\.",s,
		ignore.case=TRUE) > 0) {
		splits = c(".or.",".and.")
		lassign = TRUE
	} else if (regexpr("^\\t*(.+ = )?(\\(+|[+-])*\\w+.*[+*/-]",s) > 0) {
		splits = c("+","-","*","/","=")
	} else {
		splits = ","
	}

	resplits = gsub("([]^|().+*?${}[])","\\\\\\1",splits)
	split = paste(resplits,collapse="|")
	nx = max(nchar(splits))
	l = strsplit(s,split)[[1]]
	ind = which(cumsum(nchar(l)+nx)+(Gtabs-1)*nt <= Gwidth)
	if (length(ind) == 0) return(s)

	# pas de coupure de tableaux, fonctions, etc.
	if (identical(splits,",")) {
		arr = ",\\<[a-z][[:alnum:]_%]*\\(([^()]+|\\([^()]+\\))+$"
		while (length(ind) > 1 &&
			regexpr(arr,paste(l[ind],collapse=","),ignore.case=TRUE) > 0) {
			ind = ind[-length(ind)]
		}
	}

	lre = gsub("([]^|()/\\.+*?${}[])","\\\\\\1",l[ind])
	patt = sprintf("(%s(?:%s))(.*)",paste(lre,collapse=sprintf("(?:%s)",split)),
		split)
	ire = regexec(patt,s)
	ms = regmatches(s,ire)[[1]]
	s1 = sprintf("%s&\n",ms[2])
	s2 = sprintf("%s%s",paste(rep("\t",nt+ntab),collapse=""),ms[length(ms)])

	paste(s1,splitLine(s2,0,call,lassign),sep="")
}

resize = function(lignes)
{
	for (i in seq(along=lignes)) lignes[i] = splitLine(lignes[i])

	unlist(strsplit(paste(lignes,collapse="\n"),split="\n"))
}

#keys = sprintf("associate|%s|namelist|common|equivalence|(block )?data",types)
array = "\\<([a-z][[:alnum:]_%]*)(%[a-z]\\w*|\\(([^()]+|\\(([^()]+|\\([^()]+\\))+\\))+\\))*"
fun1 = "abs|exp|log|a?cos|a?sin|a?tan|pow|min|max|mod|modulo|sum"
fun2 = "all|any|isnan|count|real|n?int|sign|huge|tiny|size|shape|trim"
fun = paste(fun1,fun2,sep="|")
#arg = sprintf("%s|\\d+|'[^']+'",array)
num = "\\<(\\d+(\\.\\d*)?|\\.\\d+)(_\\w+)?([ed][+-]?\\d+)?"
#oper = sprintf("%s|%s",array,num)

findarrays = function(s)
{
	ms = regmatches(s,gregexpr(array,s,ignore.case=TRUE))[[1]]
	if (length(ms) == 0) return(gsub(num,"\\1\\4",s))

	arrays = sub(sprintf("(%s)\\((.+,)?%s(,.+)?\\)",fun,array),"\\3",ms)
	sub(array,"\\1",arrays)
}

ll = "^(\\t*(if \\(.+\\) +)?)(\\w+ *=.*\\.(and|x?or|not)\\..+)"
alt = "^(\\t*)else$"
mem = "^\\t*(associate|(if \\(.+\\) )?(de)?allocate) *\\("
loop = "^(\\t*do) \\w+=.+"
testa = "^(\\t*)(else )?if \\((.+)\\) +then"
testb = "^(\\t*)if \\((.+)\\) +(\\w+.+)"
call = "^(\\t*call \\w+)\\((.+)\\)"
seta = sprintf("^(\\t*)%s = *(\\(+|%s[*/+-]+)*(.+)",array,num)

algo = function(line)
{
	if (regexpr("^\\t*(contains|subroutine|(?!end).*\\bfunction)\\b",line,
		perl=TRUE) > 0 || regexpr(alt,line) > 0 || regexpr(ll,line) > 0) {
		return(line)
	}

	if (regexpr(mem,line) > 0) {
		line = ""
	} else if (regexpr(loop,line) > 0) {
		line = sub(loop,"\\1",line)
	} else if (regexpr(testa,line) > 0) {
		line = sub(testa,"\\1\\2if (\\3)",line)
	} else if (regexpr(testb,line) > 0) {
		l = algo(sub(testb,"\\3",line))
		line = paste(sub(testb,"\\1if (\\2)\n\\1",line),l,sep="\t")
	} else if (regexpr(call,line) > 0) {
		s = regmatches(line,regexec(call,line))[[1]]
		s[3] = gsub("(^|,)\\w=","",s[3])
		args = findarrays(s[3])
		line = sprintf("%s(%s)",s[2],paste(args,collapse=","))
	} else if (regexpr(seta,line) > 0) {
		s = regmatches(line,regexec(seta,line))[[1]]
		ops = findarrays(s[length(s)])

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
	texte = paste(flines,collapse="\n")
	texte = stripcont(texte)
	texte = mergecont(texte)
	for (re in lre)
		texte = gsub(re[1],re[2],texte,ignore.case=TRUE,perl=re[3],useBytes=TRUE)
	stopifnot(regexpr("\\t| \n",texte) < 0)

	# remplacement définitif ',;' (fin protection espacement ',')
	texte = gsub(",;",",",texte)

	texte = deloop(texte)
	texte = deif(texte)
	texte = with(vars,rename(texte,used,decl,asso,occ,replace))

	flines = strsplit(texte,"\n")[[1]]
	flines = declarassume(flines)
	flines = orderlocal(flines)

	texte = paste(flines,collapse="\n")
	texte = declarmerge(texte)
	flines = strsplit(texte,"\n")[[1]]
	#flines = stripblocks(flines)

	flines = cleanasso(flines)

	if (Gtabs == 0) {
		for (i in seq(along=flines)) flines[i] = sub("^ *","",flines[i])
	} else {
		flines = reindent(flines)
		stopifnot(all(regexpr("^ *\\t* +|\n",flines) < 0))

		if (Gwidth > 10) flines = resize(flines)
	}

	stopifnot(all(regexpr("^ ",flines) < 0))
	flines
}

getdoc = function(flines)
{
	texte = paste(flines,collapse="\n")
	tt1 = tt2 = ""

	unit = "(^|\\s+)(module|program)\\s(.+)\n\\s*(contains|end)\\>"
	ire = regexec(unit,texte,ignore.case=TRUE)
	if (ire[[1]][1] > 0) tt1 = regmatches(texte,ire)[[1]][4]

	doc = "\n *!\\*{3,5} \\*\\w+[^\n]+(\n+ *![^\n]+)+"
	ire = gregexpr(doc,texte,ignore.case=TRUE)
	if (ire[[1]][1] > 0) tt2 = regmatches(texte,ire)[[1]]

	lines = unlist(strsplit(paste(tt1,tt2,sep="\n"),"\n+"))
	lines = grep("^\\s*!",lines,value=TRUE)
	lines = grep("^\\s*!+((dir|dec|pgi)\\$|\\$[^!]|ocl |[ ~=*_+$!/-^]*$)",lines,
		ignore.case=TRUE,invert=TRUE,value=TRUE)

	gsub("^\\s*! *","",lines)
}

getalgo = function(flines)
{
	flines = rewrite(flines)

	subin = "^\\t*(subroutine|(?!end)[^\n!]*function)\\b"
	subout = "^\\t*end (subroutine|function)\\>"
	isin1 = isin2 = FALSE
	for (i in seq(along=flines)) {
		# isin2 (avant) : isin1 and (isin2 or entering) and not outing
		# isin1 : (isin1 or entering) and (isin2 or not outing)
		isin2 = isin1 && (isin2 || regexpr(subin,flines[i],perl=TRUE) > 0) &&
			regexpr(subout,flines[i]) < 0
		isin1 = (isin1 || regexpr(subin,flines[i],perl=TRUE) > 0) &&
			(isin2 || regexpr(subout,flines[i]) < 0)

		if (! isin1) {
			flines[i] = ""
			next
		}

		flines[i] = algo(flines[i])
	}

	stopifnot(! isin1 && ! isin2)

	texte = paste(flines,collapse="\n")
	texte = gsub("(\n\\t*)(do\n+\\t*)+","\\1loop: ",texte)
	strsplit(texte,split="\n+")[[1]]
}

getintfb = function(flines,fun)
{
	texte = paste(flines,collapse="\n")
	texte = stripcont(texte)
	texte = mergecont(texte)
	for (re in lre)
		texte = gsub(re[1],re[2],texte,ignore.case=TRUE,perl=re[3],useBytes=TRUE)
	stopifnot(regexpr("\\t| \n",texte) < 0)

	# remplacement définitif ',;' (fin protection espacement ',')
	texte = gsub(",;",",",texte)
	flines = strsplit(texte,"\n")[[1]]

	re1 = "^\\s*(program|module|submodule *\\( *\\w+ *\\)) +\\w+"
	if (any(regexpr(re1,flines,ignore.case=TRUE) > 0)) stop("intfb in no plain source")

	re2 = sprintf("^\\s*(subroutine|((%s).*? +)?function) +%s\\>",types,fun)
	ind = grep(re2,flines,ignore.case=TRUE)
	if (length(ind) == 0) stop(sprintf("procedure %s not found",fun))
	if (length(ind) > 1) warning(sprintf("several procedures %s found, use 1st one",fun))

	re = "^\\s*(end +(subroutine|function)|contains)\\>"
	inde = grep(re,flines,ignore.case=TRUE)
	stopifnot(length(inde) > 0)

	ie = inde[inde > ind[1]]
	stopifnot(length(ie) > 0)

	re3 = "^\\s*(use \\w+|implicit\\s+none)"
	re4 = sprintf("^\\s*(%s)\\>.*,\\s*intent\\((in|out|inout)\\)",types)
	re = paste(re2,re3,re4,sep="|")
	fsub = flines[ind[1]:ie[1]-1]
	indi = grep(sprintf("^\\s*(%s)",re),fsub,ignore.case=TRUE)
	if (regexpr("^\\s*contains\\>",flines[ie[1]],ignore.case=TRUE) > 0) {
		f = sub("^\\s*(subroutine|function)\\s+(\\w+).*","\\1",flines[ind[1]],
			ignore.case=TRUE)
		endf = paste("END",f)
	} else {
		endf = flines[ie[1]]
	}

	c("interface",fsub[indi],endf,"end interface")
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
	decl=sprintf("\n *\\<(%s)\\>[^\n]*::[^\n]*\\<%s\\>",types,vars$old),
	asso=sprintf("\n *associate\\([^\n]*\\<%s=>",vars$old),
	occ=sprintf("([^%%])\\<%s\\>",vars$old),replace=sprintf("\\1%s",vars$new))

if (interactive()) browser()

args = strsplit(commandArgs(trailingOnly=TRUE),split="=")
cargs = lapply(args,function(x) unlist(strsplit(x[-1],split=":")))
names(cargs) = sapply(args,function(x) x[1])

if ("force" %in% names(cargs) && as.logical(cargs$force)) options(warn=2)

if (is.null(cargs$opt) || cargs$opt == "rewrite") {
	action = rewrite
} else if (cargs$opt == "doc") {
	action = getdoc
} else if (cargs$opt == "algo") {
	action = getalgo
} else if (cargs$opt == "call") {
	action = getcall
} else if (cargs$opt == "intfb") {
	action = function(f) getintfb(f,cargs$fun)
} else {
	stop("action '",cargs$opt,"' inconnue")
}

Glower = TRUE
if ("lower" %in% names(cargs)) Glower = as.logical(cargs$lower)

Gwidth = 90
if ("width" %in% names(cargs)) Gwidth = as.integer(cargs$width)

Gtabs = 3
if ("tabs" %in% names(cargs)) Gtabs = as.integer(cargs$tabs)

verbose = FALSE
if (! is.null(cargs$verbose)) verbose = as.logical(cargs$verbose)

if (file.info(cargs$ficin)$isdir) {
	# add '\\.' to any extension, made as a REGEXP
	if (! "ext" %in% names(cargs) || ! nzchar(cargs$ext)) stop("option 'ext' must be passed")

	ext = sprintf("(\\.%s)$",gsub(":","|\\\\.",cargs$ext))
	ficin = dir(cargs$ficin,pattern=ext,full.names=TRUE)
} else {
	ficin = cargs$ficin
}

if (! file.exists(cargs$ficout))
	dir.create(dirname(cargs$ficout),showWarnings=FALSE,recursive=TRUE)

if (file.exists(cargs$ficout) && file.info(cargs$ficout)$isdir) {
	ficout = paste(cargs$ficout,basename(ficin),sep="/")
	ficout0 = NULL
	append = FALSE
} else {
	ficout = tempfile(fileext=rep(".f90",length(ficin)))
	ficout0 = cargs$ficout
	for (i in seq(along=ficout)) cat("\n! from",ficin[i],":\n",file=ficout[i])
	append = TRUE
}

stopifnot(length(ficin) == length(ficout))

nin = nout = 0

for (i in seq(along=ficin)) {
	if (verbose) cat(ficin[i],"->",ficout[i],"\n")
	flines = readLines(ficin[i])
	flines = toUTF8(flines)

	nin = nin + length(flines)

	flines = action(flines)
	if (length(flines) == 0) next

	n = length(flines)
	if (nzchar(flines[n])) flines[n+1] = ""

	nout = nout + length(flines)
	texte = paste(flines,collapse="\n")
	cat(texte,file=ficout[i],append=append)
}

if (! is.null(ficout0)) invisible(file.append(ficout0,ficout))

cat("Lignes",cargs$ficin,":",nout,"/",nin,"(=",round(nout/nin*100),"%)\n")

Gnam = "[&/$] *\\w+"
Gvar = "\\w+(%\\w+)*"
Gtab = "   "

args = strsplit(commandArgs(trailingOnly=TRUE),split="=")
cargs = lapply(args,function(x) strsplit(x[-1],split=":")[[1]])
names(cargs) = sapply(args,function(x) x[1])

if (file.info(cargs$ficnam)$isdir) {
	stopifnot(file.info(cargs$ficout)$isdir)

	fics = dir(cargs$ficnam,"\\.nam",full.names=TRUE)
	ficout = paste(cargs$ficout,basename(fics),sep="/")
} else {
	stopifnot(! file.info(cargs$ficnam)$isdir)

	fics = cargs$ficnam
	ficout = cargs$ficout
	if (file.info(ficout)$isdir) ficout = paste(ficout,basename(fics),sep="/")
}

cat("Nb of files:",length(ficout),"\n")

fcat = function(...) invisible(return(NULL))
fcat2 = function(...) invisible(return(NULL))
if ("verbose" %in% names(cargs)) {
	if (as.integer(cargs$verbose) >= 1) fcat = cat
	if (as.integer(cargs$verbose) >= 2) fcat2 = cat
}

# in delta file, namelist blocks are supposed to be fairly well organized in lines
# ie variables are separated by lines, as well as from the name of their namelist block
nd = readLines(cargs$delta)
ind = grep(sprintf("^\\s*%s",Gnam),nd,ignore.case=TRUE)
namd = gsub("^\\s*[/&](\\w+).*","\\1",nd[ind],ignore.case=TRUE)

cat("Nb of blocks:",length(namd),"\n")
if (length(namd) == 0) {
	cat("--> no namelist block to operate\n")
	quit("no")
}

ind1 = sapply(namd,function(x) grep(sprintf("^\\s*[&/$] *%s\\>",x),nd,ignore.case=TRUE))
ind2 = lapply(ind1,function(i) grep(sprintf("^\\s*%s",Gnam),nd[-(1:i)],ignore.case=TRUE))
irm = grep("! --rmblock=",nd)
if (length(irm) > 0) namrm = unlist(strsplit(sub("! --rmblock=","",nd[irm]),","))

for (ii in seq(along=fics)) {
	cat("file:",fics[ii],"\n")
	nn = readLines(fics[ii])
	nn = grep("^ *$",nn,invert=TRUE,value=TRUE)

	for (i in seq(along=namd)) {
		fcat(". namelist block:",namd[i],"\n")
		il1 = ind1[i]
		il2 = ind2[[i]]
		if (length(il2) == 0) {
			fcat2("--> last block, exit\n")
			break
		} else if (il2[1] == 1) {
			fcat2("--> no instruction, next\n")
			next
		}

		il2 = grep(sprintf("^\\s*(%s)? *(%s *=.+)?/? *$",Gnam,Gvar),nd[il1:(il1+il2[1]-1)])
		stopifnot(length(il2) > 0)

		for (j in il1:(il1+max(il2)-1)) {
			val = NULL
			isnam = FALSE

			re = sprintf("^\\s*(%s)?(%s) *= *(@?[^@=,]+?) *,?$",Gnam,Gvar)
			if (regexpr(re,nd[j],ignore.case=TRUE) > 0) {
				val = sub(re,"\\4",nd[j],ignore.case=TRUE)
				isnam = regexpr("^@",val) > 0
				val = sub("^@","",val)
			}

			iold = grep(sprintf("^\\s*[/&] *%s\\>",namd[i]),nn,ignore.case=TRUE)
			if (length(iold) == 0) {
				fcat2("--> adding namelist block",namd[i],"(empty)\n")
				nn[length(nn)+1] = sprintf(" &%s/",toupper(namd[i]))
				iold = length(nn)
			}

			stopifnot(length(iold) == 1)

			if (is.null(val)) {
				fcat2("--> no instruction in:",nd[j],"\n")
				next
			}

			var = sub(re,"\\2\\3",nd[j],ignore.case=TRUE)
			fcat(". var/val:",var,"/",val,"\n")

			jl2 = grep(sprintf("^\\s*%s",Gnam),nn[-(1:iold)],ignore.case=TRUE)
			if (length(jl2) == 0) jl2 = 1

			# in namelist file, variables in blocks can be on the same line, and even split
			# over several lines
			re = "^\\s*([&/$] *%s)?(.+,)*%s(\\([0-9]+\\))? *= *([^=,]+?)( *,.+)? *,? */? *$"
			re = sprintf(re,toupper(namd[i]),var)
			ind = grep(re,nn[iold:(iold+jl2[1]-1)],ignore.case=TRUE)

			if (isnam) {
				fcat(".. moving var",var,"from",namd[i],"to",val,"\n")
				if (length(ind) == 0) {
					fcat2("--> variable",var,"absent: no movement\n")
				} else {
					renew = sprintf("^(\\s*[&/$] *%s\\>)",val)
					inew = grep(renew,nn,ignore.case=TRUE)
					if (length(inew) == 0) {
						fcat2("--> adding namelist block (from move)",namd[i],"(empty)\n")
						nn[length(nn)+1] = sprintf(" &%s/",toupper(val))
						inew = length(nn)
					}

					if (length(ind) > 1) {
						fcat2("--> vector variable: processing whole lines\n")
						s = paste(nn[iold+ind-1],collapse="\n")
						nn[inew] = sub(renew,sprintf("\\1\n%s",s),nn[inew],ignore.case=TRUE)
					} else {
						valold = sub(re,"\\4",nn[iold+ind-1],ignore.case=TRUE)
						s = sprintf("\\1\n%s%s=%s,",Gtab,toupper(var),valold)
						nn[inew] = sub(renew,s,nn[inew],ignore.case=TRUE)
					}
				}
			} else if (val != "--") {
				fcat(".. adding variable",var,"=",val,"in nam",namd[i],"\n")
				if (length(ind) > 0) {
					fcat("--> variable",var,"already present: no replacement\n")
				} else if (val != "++") {
					reold = sprintf("^(\\s*[&/$] *%s\\>)",toupper(namd[i]))
					s = sprintf("\\1\n%s%s=%s,",Gtab,var,val)
					nn[iold] = sub(reold,s,nn[iold],ignore.case=TRUE)
				}
			}

			if (val == "--" || isnam) {
				fcat(".. deleting variable",var,"in nam",namd[i],"\n")
				if (length(ind) == 0) {
					fcat("--> variable",var,"absent: no delete\n")
				} else if (length(ind) > 1) {
					fcat2("--> vector variable: processing whole lines\n")
					nn = nn[-(iold+ind-1)]
				} else {
					valold = sub(re,"\\4",nn[iold+ind-1],ignore.case=TRUE)
					reold = sprintf(" *\\<%s *= *%s *,?",var,valold)
					# in case of merged block name and variables:
					# better insert/cut line in nn[inew/iold] than insert/cut line
					nn[iold+ind-1] = sub(reold,"",nn[iold+ind-1],ignore.case=TRUE)
				}
			}
		}

		if (length(namrm) > 0) {
			for (i in seq(along=namrm)) {
				fcat(". removing block",namrm[i],"\n")
				il1 = grep(sprintf("^\\s*[&/$] *%s",namrm[i]),nn,ignore.case=TRUE)
				if (length(il1) == 0) {
					fcat2("--> block not present\n")
					next
				}

				il2 = grep(sprintf("^\\s*%s",Gnam),nn[-(1:il1)],ignore.case=TRUE)
				if (length(il2) == 0 || il2[1] == 1) {
					nn = nn[-il1]
					next
				}

				nn = nn[-(il1:(il1+il2[1]-1))]
			}
		}
	}

	writeLines(nn[nzchar(nn)],ficout[ii])
}

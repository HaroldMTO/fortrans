getnams = function(file)
{
	nams = strsplit(grep("namelist/",readLines(file),value=TRUE),"/")
	names(nams) = sapply(nams,"[",2)
	for (i in seq(along=nams)) {
		nams[[i]] = strsplit(nams[[i]][-(1:2)],",")[[1]]
		ind = which(duplicated(nams[[i]]))
		if (length(ind) > 0) {
			message("--> duplicated variables in ",file,"@",names(nams)[i],": ",
				paste(nams[[i]][ind]," "))
			nams[[i]] = unique(sort(nams[[i]]))
		}
	}

	nams
}

args = strsplit(commandArgs(trailingOnly=TRUE),split="=")
cargs = lapply(args,function(x) strsplit(x[-1],split=":")[[1]])
names(cargs) = sapply(args,function(x) x[1])

namold = getnams(cargs$ficold)
namnew = getnams(cargs$ficnew)

nomsold = names(namold)
nomsnew = names(namnew)
ind = match(nomsnew,nomsold)

if (any(! nomsold %in% nomsnew)) {
	cat("! --rmblock=",paste(nomsold[! nomsold %in% nomsnew],collapse=","),"\n",
		sep="")
}

snam = sprintf("&%s",nomsnew)

if (as.logical(cargs$move)) {
	# variables to move in new blocks
	for (i in seq(along=namnew)) {
		varsnew = namnew[[i]]

		varsmv = character()
		for (v in varsnew) {
			iold = sapply(namold,function(x) any(x==v))

			# either in no old block, either in the same old block, if existing
			if (all(! iold) || (nomsnew[i] %in% nomsold &&
				iold[nomsold == nomsnew[i]])) next

			inew = sapply(namnew,function(x) any(x==v))
			if (length(which(inew)) > 1) {
				message("--> variable ",v," in several new blocks (not moved): ",
					paste(nomsnew[inew],collapse=" "))
			} else if (length(which(iold)) > 1) {
				message("--> variable ",v," in several old blocks (not moved): ",
					paste(nomsold[iold],collapse=" "))
			} else {
				# v moves from old to new: in namold, add "v=@namnew"
				varsmv = c(varsmv,v)
				sarr = sprintf("\t%s=@%s",v,nomsnew[i])
				j = match(nomsold[iold],nomsnew)
				if (is.na(j)) {
					# old reference block is not in new: add it to new, add sarr in old
					j = length(nomsnew)+1
					nomsnew[j] = nomsold[iold]
					snam[j] = sprintf("&%s\n%s",nomsold[iold],sarr)
				} else {
					# old block is in new: delete v in old (if present)
					snam[j] = sprintf("%s\n%s",snam[j],sarr)
					# delete variable in old block (before adding again by way of snam)
					k = which(iold)
					namold[[k]] = namold[[k]][namold[[k]] != v]
				}
			}
		}

		if (length(varsmv) > 0) {
			cat("! vars moved to",nomsnew[i],":",
				paste(varsnew[varsnew %in% varsmv]),"\n")
			namnew[[i]] = namnew[[i]][! namnew[[i]] %in% varsmv]
		}
	}
}

# namelist blocks that already exist in old
for (i in which(! is.na(ind))) {
	varsnew = namnew[[i]]
	varsold = namold[[ind[i]]]

	if (any(! varsold %in% varsnew)) {
		ssup = sprintf("\t%s=--",varsold[! varsold %in% varsnew])
		snam[i] = sprintf("%s\n%s",snam[i],paste(ssup,collapse="\n"))
	}

	if (any(! varsnew %in% varsold)) {
		sadd = sprintf("\t%s=++",varsnew[! varsnew %in% varsold])
		snam[i] = sprintf("%s\n%s",snam[i],paste(sadd,collapse="\n"))
	}

	if (regexpr("\n",snam[i]) < 0) next

	snam[i] = sprintf("%s\n/\n",snam[i])
	cat(snam[i])
}

# fully new namelist blocks
for (i in which(is.na(ind))) {
	varsnew = namnew[[i]]

	if (length(varsnew) > 0) {
		sadd = sprintf("\t%s=++",varsnew)
		snam[i] = sprintf("%s\n%s",snam[i],paste(sadd,collapse="\n"))
	}

	if (regexpr("\n",snam[i]) < 0) next

	snam[i] = sprintf("%s\n/\n",snam[i])
	cat(snam[i])
}

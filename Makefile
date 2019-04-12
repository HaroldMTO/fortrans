MAKEFLAGS += --no-print-directory

# ne pas mettre ~ pour P : il faut un chemin absolu
P = $(HOME)/proc/fortrans
B = ~/bin

.PHONY: all install fortrans

all:

install:
	! git status --porcelain 2>/dev/null | grep -qvE "^\?\? "
	make fortrans
	make $B/rewrite.sh
	if git status >/dev/null 2>&1; then \
		grep -q $(shell git log -1 --pretty=format:%h 2>/dev/null) $P/version || \
			git log -1 --oneline >> $P/version; \
	fi

fortrans:
	mkdir -p $P
	cp -uv re_to90.txt rename.txt $P
	sed -re "s:fortrans *=.+:fortrans = \"$P\":" rewrite.R > $P/rewrite.R

$B/rewrite.sh: rewrite.sh
	sed -re "s:fortrans=.+:fortrans=$P:" rewrite.sh | \
		iconv -f LATIN1 -t UTF8 > $B/rewrite.sh
	chmod a+x $B/rewrite.sh

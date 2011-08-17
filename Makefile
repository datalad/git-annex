PREFIX=/usr
IGNORE=-ignore-package monads-fd
GHCFLAGS=-O2 -Wall $(IGNORE) -fspec-constr-count=5
ifdef PROFILE
GHCFLAGS=-prof -auto-all -rtsopts -caf-all -fforce-recomp $(IGNORE)
endif
GHCMAKE=ghc $(GHCFLAGS) --make

bins=git-annex git-annex-shell git-union-merge
mans=git-annex.1 git-annex-shell.1 git-union-merge.1

all: $(bins) $(mans) docs

sources: SysConfig.hs StatFS.hs Touch.hs Remote/S3.hs

SysConfig.hs: configure.hs TestConfig.hs
	$(GHCMAKE) configure
	./configure

%.hs: %.hsc
	hsc2hs $<
	perl -i -pe 's/^{-# INCLUDE.*//' $@

Remote/S3.hs:
	@ln -sf S3real.hs Remote/S3.hs

Remote/S3.o: Remote/S3.hs
	@if ! $(GHCMAKE) Remote/S3.hs; then \
		ln -sf S3stub.hs Remote/S3.hs; \
		echo "** building without S3 support"; \
	fi

$(bins): SysConfig.hs Touch.hs StatFS.hs Remote/S3.o
	$(GHCMAKE) $@

git-annex.1: doc/git-annex.mdwn
	./mdwn2man git-annex 1 doc/git-annex.mdwn > git-annex.1
git-annex-shell.1: doc/git-annex-shell.mdwn
	./mdwn2man git-annex-shell 1 doc/git-annex-shell.mdwn > git-annex-shell.1
git-union-merge.1: doc/git-union-merge.mdwn
	./mdwn2man git-union-merge 1 doc/git-union-merge.mdwn > git-union-merge.1

install: all
	install -d $(DESTDIR)$(PREFIX)/bin
	install $(bins) $(DESTDIR)$(PREFIX)/bin
	install -d $(DESTDIR)$(PREFIX)/share/man/man1
	install -m 0644 $(mans) $(DESTDIR)$(PREFIX)/share/man/man1
	install -d $(DESTDIR)$(PREFIX)/share/doc/git-annex
	if [ -d html ]; then \
		rsync -a --delete html/ $(DESTDIR)$(PREFIX)/share/doc/git-annex/html/; \
	fi

test: $(bins)
	@if ! $(GHCMAKE) -O0 test; then \
		echo "** not running test suite" >&2; \
	else \
		./test; \
	fi

testcoverage: $(bins)
	rm -f test.tix test
	ghc -odir build/test -hidir build/test $(GHCFLAGS) --make -fhpc test
	./test
	@echo ""
	@hpc report test --exclude=Main --exclude=QC
	@hpc markup test --exclude=Main --exclude=QC --destdir=.hpc >/dev/null

# If ikiwiki is available, build static html docs suitable for being
# shipped in the software package.
ifeq ($(shell which ikiwiki),)
IKIWIKI=@echo "** ikiwiki not found, skipping building docs" >&2; true
else
IKIWIKI=ikiwiki
endif

docs: $(mans)
	$(IKIWIKI) doc html -v --wikiname git-annex --plugin=goodstuff \
		--no-usedirs --disable-plugin=openid --plugin=sidebar \
		--underlaydir=/dev/null --disable-plugin=shortcut \
		--disable-plugin=smiley \
		--plugin=comments --set comments_pagespec="*" \
		--exclude='news/.*'

clean:
	rm -rf build $(bins) $(mans) test configure  *.tix .hpc \
		StatFS.hs Touch.hs SysConfig.hs Remote/S3.hs
	rm -rf doc/.ikiwiki html dist
	find . \( -name \*.o -or -name \*.hi \) -exec rm {} \;

# Workaround for cabal sdist not running Setup hooks, so I cannot
# generate a file list there.
sdist: clean
	@if [ ! -e git-annex.cabal.orig ]; then cp git-annex.cabal git-annex.cabal.orig; fi
	@sed -e "s!\(Extra-Source-Files: \).*!\1$(shell find . -name .git -prune -or -not -name \\*.orig -not -type d -print | perl -ne 'print unless length >= 100')!i" < git-annex.cabal.orig > git-annex.cabal
	@cabal sdist
	@mv git-annex.cabal.orig git-annex.cabal

.PHONY: $(bins) test install

CFLAGS=-Wall
GIT_ANNEX_TMP_BUILD_DIR?=tmp
BASEFLAGS=-Wall -outputdir $(GIT_ANNEX_TMP_BUILD_DIR) -IUtility

# If you get build failures due to missing haskell libraries,
# you can turn off some of these features.
#
# If you're using an old version of yesod, enable -DWITH_OLD_YESOD
FEATURES?=$(GIT_ANNEX_LOCAL_FEATURES) -DWITH_ASSISTANT -DWITH_S3 -DWITH_WEBDAV -DWITH_WEBAPP -DWITH_PAIRING -DWITH_XMPP -DWITH_DNS -DWITH_GLOB

bins=git-annex
mans=git-annex.1 git-annex-shell.1
sources=Build/SysConfig.hs Utility/Touch.hs Utility/Mounts.hs
all=$(bins) $(mans) docs

OS?=$(shell uname | sed 's/[-_].*//')
ifeq ($(OS),Android)
OPTFLAGS?=-DWITH_INOTIFY -DWITH_ANDROID
clibs=Utility/libdiskfree.o Utility/libmounts.o
CFLAGS:=-Wall -DWITH_ANDROID
THREADFLAGS=-threaded
else
ifeq ($(OS),Linux)
OPTFLAGS?=-DWITH_INOTIFY -DWITH_DBUS
clibs=Utility/libdiskfree.o Utility/libmounts.o
THREADFLAGS=$(shell if test -e  `ghc --print-libdir`/libHSrts_thr.a; then printf -- -threaded; fi)
else
ifeq ($(OS),SunOS)
# Solaris is not supported by the assistant or watch command.
FEATURES:=$(shell echo $(FEATURES) | sed -e 's/-DWITH_ASSISTANT//' -e 's/-DWITH_WEBAPP//')
else
# BSD system
THREADFLAGS=-threaded
ifeq ($(OS),Darwin)
# use fsevents for OSX
OPTFLAGS?=-DWITH_FSEVENTS
clibs=Utility/libdiskfree.o Utility/libmounts.o
# Ensure OSX compiler builds for 32 bit when using 32 bit ghc
GHCARCH:=$(shell ghc -e 'print System.Info.arch')
ifeq ($(GHCARCH),i386)
CFLAGS=-Wall -m32
endif
else
# BSD system with kqueue
OPTFLAGS?=-DWITH_KQUEUE
clibs=Utility/libdiskfree.o Utility/libmounts.o Utility/libkqueue.o
endif
endif
endif
endif

ALLFLAGS = $(BASEFLAGS) $(FEATURES) $(OPTFLAGS) $(THREADFLAGS)

PREFIX=/usr
GHCFLAGS=-O2 $(ALLFLAGS)

ifdef PROFILE
GHCFLAGS=-prof -auto-all -rtsopts -caf-all -fforce-recomp $(ALLFLAGS)
endif

GHC?=ghc
GHCMAKE=$(GHC) $(GHCFLAGS) --make

# Am I typing :make in vim? Do a fast build.
ifdef VIM
all=fast
endif

build: $(all)
	touch build-stamp

sources: $(sources)

# Disables optimisation. Not for production use.
fast: GHCFLAGS=$(ALLFLAGS)
fast: $(bins)

Build/SysConfig.hs: configure.hs Build/TestConfig.hs Build/Configure.hs
	$(GHCMAKE) configure
	./configure $(OS)

%.hs: %.hsc
	hsc2hs $<

git-annex: $(sources) $(clibs)
	install -d $(GIT_ANNEX_TMP_BUILD_DIR)
	$(GHCMAKE) $@ -o $(GIT_ANNEX_TMP_BUILD_DIR)/git-annex $(clibs)
	ln -sf $(GIT_ANNEX_TMP_BUILD_DIR)/git-annex git-annex

git-annex.1: doc/git-annex.mdwn
	./Build/mdwn2man git-annex 1 doc/git-annex.mdwn > git-annex.1
git-annex-shell.1: doc/git-annex-shell.mdwn
	./Build/mdwn2man git-annex-shell 1 doc/git-annex-shell.mdwn > git-annex-shell.1
git-union-merge.1: doc/git-union-merge.mdwn
	./Build/mdwn2man git-union-merge 1 doc/git-union-merge.mdwn > git-union-merge.1

install-mans: $(mans)
	install -d $(DESTDIR)$(PREFIX)/share/man/man1
	install -m 0644 $(mans) $(DESTDIR)$(PREFIX)/share/man/man1

install-docs: docs install-mans
	install -d $(DESTDIR)$(PREFIX)/share/doc/git-annex
	if [ -d html ]; then \
		rsync -a --delete html/ $(DESTDIR)$(PREFIX)/share/doc/git-annex/html/; \
	fi

install: build-stamp install-docs
	install -d $(DESTDIR)$(PREFIX)/bin
	install $(bins) $(DESTDIR)$(PREFIX)/bin
	ln -sf git-annex $(DESTDIR)$(PREFIX)/bin/git-annex-shell
	runghc Build/InstallDesktopFile.hs $(PREFIX)/bin/git-annex || true

test: $(sources) $(clibs)
	@if ! $(GHCMAKE) -O0 test $(clibs); then \
		echo "** failed to build the test suite" >&2; \
		exit 1; \
	elif ! ./test; then \
		echo "** test suite failed!" >&2; \
		exit 1; \
	fi

testcoverage:
	rm -f test.tix test
	$(GHC) $(GHCFLAGS) -outputdir $(GIT_ANNEX_TMP_BUILD_DIR)/testcoverage --make -fhpc test
	./test
	@echo ""
	@hpc report test --exclude=Main --exclude=QC
	@hpc markup test --exclude=Main --exclude=QC --destdir=.hpc >/dev/null
	@echo "(See .hpc/ for test coverage details.)"

# hothasktags chokes on some tempolate haskell etc, so ignore errors
tags:
	find . | grep -v /.git/ | grep -v /doc/ | egrep '\.hs$$' | xargs hothasktags > tags 2>&1

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
		--exclude='news/.*' --exclude='design/assistant/blog/*' \
		--exclude='bugs/*' --exclude='todo/*' --exclude='forum/*'

clean:
	rm -rf $(GIT_ANNEX_TMP_BUILD_DIR) $(bins) $(mans) test configure  *.tix .hpc $(sources) \
		doc/.ikiwiki html dist $(clibs) build-stamp tags

sdist: clean $(mans)
	./Build/make-sdist.sh

# Upload to hackage.
hackage: sdist
	@cabal upload dist/*.tar.gz

LINUXSTANDALONE_DEST=$(GIT_ANNEX_TMP_BUILD_DIR)/git-annex.linux
linuxstandalone:
	$(MAKE) git-annex

	rm -rf "$(LINUXSTANDALONE_DEST)"

	cp -R standalone/linux "$(LINUXSTANDALONE_DEST)"

	install -d "$(LINUXSTANDALONE_DEST)/bin"
	cp git-annex "$(LINUXSTANDALONE_DEST)/bin/"
	strip "$(LINUXSTANDALONE_DEST)/bin/git-annex"
	ln -sf git-annex "$(LINUXSTANDALONE_DEST)/bin/git-annex-shell"
	zcat standalone/licences.gz > $(LINUXSTANDALONE_DEST)/LICENSE

	runghc Build/Standalone.hs "$(LINUXSTANDALONE_DEST)"
	
	install -d "$(LINUXSTANDALONE_DEST)/git-core"
	(cd "$(shell git --exec-path)" && tar c .) | (cd "$(LINUXSTANDALONE_DEST)"/git-core && tar x)
	install -d "$(LINUXSTANDALONE_DEST)/templates"
	
	touch "$(LINUXSTANDALONE_DEST)/libdirs.tmp"
	for lib in $$(ldd "$(LINUXSTANDALONE_DEST)"/bin/* $$(find "$(LINUXSTANDALONE_DEST)"/git-core/ -type f) | grep -v -f standalone/linux/glibc-libs | grep -v "not a dynamic executable" | egrep '^	' | sed 's/^\t//' | sed 's/.*=> //' | cut -d ' ' -f 1 | sort | uniq); do \
		dir=$$(dirname "$$lib"); \
		install -d "$(LINUXSTANDALONE_DEST)/$$dir"; \
		echo "$$dir" >> "$(LINUXSTANDALONE_DEST)/libdirs.tmp"; \
		cp "$$lib" "$(LINUXSTANDALONE_DEST)/$$dir"; \
		if [ -L "$lib" ]; then \
			link=$$(readlink -f "$$lib"); \
			cp "$$link" "$(LINUXSTANDALONE_DEST)/$$(dirname "$$link")"; \
		fi; \
	done
	sort "$(LINUXSTANDALONE_DEST)/libdirs.tmp" | uniq > "$(LINUXSTANDALONE_DEST)/libdirs"
	rm -f "$(LINUXSTANDALONE_DEST)/libdirs.tmp"

	cd $(GIT_ANNEX_TMP_BUILD_DIR) && tar czf git-annex-standalone-$(shell dpkg --print-architecture).tar.gz git-annex.linux

OSXAPP_DEST=$(GIT_ANNEX_TMP_BUILD_DIR)/build-dmg/git-annex.app
OSXAPP_BASE=$(OSXAPP_DEST)/Contents/MacOS
osxapp:
	$(MAKE) git-annex

	rm -rf "$(OSXAPP_DEST)"
	install -d $(GIT_ANNEX_TMP_BUILD_DIR)/build-dmg
	cp -R standalone/osx/git-annex.app "$(OSXAPP_DEST)"

	install -d "$(OSXAPP_BASE)"
	cp git-annex "$(OSXAPP_BASE)"
	strip "$(OSXAPP_BASE)/git-annex"
	ln -sf git-annex "$(OSXAPP_BASE)/git-annex-shell"
	gzcat standalone/licences.gz > $(OSXAPP_BASE)/LICENSE
	cp $(OSXAPP_BASE)/LICENSE $(GIT_ANNEX_TMP_BUILD_DIR)/build-dmg/LICENSE.txt

	runghc Build/Standalone.hs $(OSXAPP_BASE)

	(cd "$(shell git --exec-path)" && tar c .) | (cd "$(OSXAPP_BASE)" && tar x)
	install -d "$(OSXAPP_BASE)/templates"

	runghc Build/OSXMkLibs.hs $(OSXAPP_BASE)
	rm -f tmp/git-annex.dmg
	hdiutil create -size 640m -format UDRW -srcfolder $(GIT_ANNEX_TMP_BUILD_DIR)/build-dmg \
		-volname git-annex -o tmp/git-annex.dmg
	rm -f tmp/git-annex.dmg.bz2
	bzip2 --fast tmp/git-annex.dmg

# Cross compile for Android binary.
# Uses https://github.com/neurocyte/ghc-android
#
# configure is run, probing the local system.
# So the Android should have all the same stuff that configure probes for,
# including the same version of git.
android:
	OS=Android $(MAKE) Build/SysConfig.hs
	GHC=$$HOME/.ghc/android-14/arm-linux-androideabi-4.7/bin/arm-unknown-linux-androideabi-ghc \
	CC=$$HOME/.ghc/android-14/arm-linux-androideabi-4.7/bin/arm-linux-androideabi-gcc \
	FEATURES="-DWITH_ANDROID -DWITH_ASSISTANT -DWITH_GLOB -DWITH_DNS" \
	OS=Android $(MAKE) fast

ANDROIDAPP_DEST=$(GIT_ANNEX_TMP_BUILD_DIR)/git-annex.android
androidapp:
	$(MAKE) android
	$(MAKE) -C standalone/android
	cp standalone/android/source/term/bin/Term-debug.apk $(GIT_ANNEX_TMP_BUILD_DIR)/GitAnnex.apk

# used by ./ghci
getflags:
	@echo $(ALLFLAGS) $(clibs)

.PHONY: $(bins) test install tags

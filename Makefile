mans=git-annex.1 git-annex-shell.1
all=git-annex $(mans) docs

GHC?=ghc
PREFIX?=/usr
CABAL?=cabal # set to "./Setup" if you lack a cabal program

# Am I typing :make in vim? Do a fast build.
ifdef VIM
all=fast
endif

build: build-stamp
build-stamp: $(all)
	touch $@

Build/SysConfig.hs: configure.hs Build/TestConfig.hs Build/Configure.hs
	if [ "$(CABAL)" = ./Setup ]; then ghc --make Setup; fi
	$(CABAL) configure

git-annex: Build/SysConfig.hs
	$(CABAL) build
	ln -sf dist/build/git-annex/git-annex git-annex

git-annex.1: doc/git-annex.mdwn
	./Build/mdwn2man git-annex 1 doc/git-annex.mdwn > git-annex.1
git-annex-shell.1: doc/git-annex-shell.mdwn
	./Build/mdwn2man git-annex-shell 1 doc/git-annex-shell.mdwn > git-annex-shell.1

# These are not built normally.
git-union-merge.1: doc/git-union-merge.mdwn
	./Build/mdwn2man git-union-merge 1 doc/git-union-merge.mdwn > git-union-merge.1
git-union-merge:
	$(GHC) --make -threaded $@

SHAREDIR=share

install-mans: $(mans)
	install -d $(DESTDIR)$(PREFIX)/$(SHAREDIR)/man/man1
	install -m 0644 $(mans) $(DESTDIR)$(PREFIX)/$(SHAREDIR)/man/man1

install-docs: docs install-mans
	install -d $(DESTDIR)$(PREFIX)/$(SHAREDIR)/doc/git-annex
	if [ -d html ]; then \
		rsync -a --delete html/ $(DESTDIR)$(PREFIX)/$(SHAREDIR)/doc/git-annex/html/; \
	fi

install: build install-docs Build/InstallDesktopFile
	install -d $(DESTDIR)$(PREFIX)/bin
	install git-annex $(DESTDIR)$(PREFIX)/bin
	ln -sf git-annex $(DESTDIR)$(PREFIX)/bin/git-annex-shell
	./Build/InstallDesktopFile $(PREFIX)/bin/git-annex || true

test: git-annex
	./git-annex test

# hothasktags chokes on some template haskell etc, so ignore errors
tags:
	find . | grep -v /.git/ | grep -v /tmp/ | grep -v /dist/ | grep -v /doc/ | egrep '\.hs$$' | xargs hothasktags > tags 2>/dev/null

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
	rm -rf tmp dist git-annex $(mans) configure  *.tix .hpc \
		doc/.ikiwiki html dist tags Build/SysConfig.hs build-stamp \
		Setup Build/InstallDesktopFile Build/EvilSplicer \
		Build/Standalone Build/OSXMkLibs Build/DistributionUpdate \
		git-union-merge
	find . -name \*.o -exec rm {} \;
	find . -name \*.hi -exec rm {} \;

Build/InstallDesktopFile: Build/InstallDesktopFile.hs
	$(GHC) --make $@
Build/EvilSplicer: Build/EvilSplicer.hs
	$(GHC) --make $@
Build/Standalone: Build/Standalone.hs Build/SysConfig.hs
	$(GHC) --make $@
Build/OSXMkLibs: Build/OSXMkLibs.hs
	$(GHC) --make $@

sdist: clean $(mans)
	./Build/make-sdist.sh

# Upload to hackage.
hackage: sdist
	@cabal upload dist/*.tar.gz

LINUXSTANDALONE_DEST=tmp/git-annex.linux
linuxstandalone: Build/Standalone
	$(MAKE) git-annex

	rm -rf "$(LINUXSTANDALONE_DEST)"
	mkdir -p tmp
	cp -R standalone/linux "$(LINUXSTANDALONE_DEST)"
	
	install -d "$(LINUXSTANDALONE_DEST)/bin"
	cp git-annex "$(LINUXSTANDALONE_DEST)/bin/"
	strip "$(LINUXSTANDALONE_DEST)/bin/git-annex"
	ln -sf git-annex "$(LINUXSTANDALONE_DEST)/bin/git-annex-shell"
	zcat standalone/licences.gz > $(LINUXSTANDALONE_DEST)/LICENSE
	cp doc/favicon.png doc/logo.svg $(LINUXSTANDALONE_DEST)

	./Build/Standalone "$(LINUXSTANDALONE_DEST)"
	
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
	
	$(MAKE) install-mans DESTDIR="$(LINUXSTANDALONE_DEST)"

	cd tmp/git-annex.linux && find . -type f > git-annex.MANIFEST
	cd tmp/git-annex.linux && find . -type l >> git-annex.MANIFEST
	cd tmp && tar czf git-annex-standalone-$(shell dpkg --print-architecture).tar.gz git-annex.linux

OSXAPP_DEST=tmp/build-dmg/git-annex.app
OSXAPP_BASE=$(OSXAPP_DEST)/Contents/MacOS/bundle
osxapp: Build/Standalone Build/OSXMkLibs
	$(MAKE) git-annex

	rm -rf "$(OSXAPP_DEST)"
	install -d tmp/build-dmg
	cp -R standalone/osx/git-annex.app "$(OSXAPP_DEST)"

	install -d "$(OSXAPP_BASE)"
	cp git-annex "$(OSXAPP_BASE)"
	strip "$(OSXAPP_BASE)/git-annex"
	ln -sf git-annex "$(OSXAPP_BASE)/git-annex-shell"
	gzcat standalone/licences.gz > $(OSXAPP_BASE)/LICENSE
	cp $(OSXAPP_BASE)/LICENSE tmp/build-dmg/LICENSE.txt

	./Build/Standalone $(OSXAPP_BASE)

	(cd "$(shell git --exec-path)" && tar c .) | (cd "$(OSXAPP_BASE)" && tar x)
	install -d "$(OSXAPP_BASE)/templates"

	# OSX looks in man dir nearby the bin
	$(MAKE) install-mans DESTDIR="$(OSXAPP_BASE)" SHAREDIR=""

	./Build/OSXMkLibs $(OSXAPP_BASE)
	cd $(OSXAPP_DEST) && find . -type f > Contents/MacOS/git-annex.MANIFEST
	cd $(OSXAPP_DEST) && find . -type l >> Contents/MacOS/git-annex.MANIFEST
	rm -f tmp/git-annex.dmg
	hdiutil create -format UDBZ -srcfolder tmp/build-dmg \
		-volname git-annex -o tmp/git-annex.dmg

ANDROID_FLAGS?=
# Cross compile for Android.
# Uses https://github.com/neurocyte/ghc-android
android: Build/EvilSplicer
	echo "Running native build, to get TH splices.."
	if [ ! -e dist/setup/setup ]; then $(CABAL) configure -f-Production -O0 $(ANDROID_FLAGS) -fAndroidSplice;  fi
	mkdir -p tmp
	if ! $(CABAL) build --ghc-options=-ddump-splices 2> tmp/dump-splices; then tail tmp/dump-splices >&2; exit 1; fi
	echo "Setting up Android build tree.."
	./Build/EvilSplicer tmp/splices tmp/dump-splices standalone/android/evilsplicer-headers.hs
	rsync -az --exclude tmp --exclude dist . tmp/androidtree
# Copy the files with expanded splices to the source tree, but
# only if the existing source file is not newer. (So, if a file
# used to have TH splices but they were removed, it will be newer,
# and not overwritten.)
	cp -uR tmp/splices/* tmp/androidtree || true
# Some additional dependencies needed by the expanded splices.
	sed -i 's/^  Build-Depends: /  Build-Depends: yesod-routes, yesod-core, shakespeare-css, shakespeare-js, shakespeare, blaze-markup, file-embed, wai-app-static, /' tmp/androidtree/git-annex.cabal
# Avoid warnings due to sometimes unused imports added for the splices.
	sed -i 's/GHC-Options: \(.*\)-Wall/GHC-Options: \1-Wall -fno-warn-unused-imports /i' tmp/androidtree/git-annex.cabal
# Cabal cannot cross compile with custom build type, so workaround.
	sed -i 's/Build-type: Custom/Build-type: Simple/' tmp/androidtree/git-annex.cabal
# Build just once, but link twice, for 2 different versions of Android.
	mkdir -p tmp/androidtree/dist/build/git-annex/4.0 tmp/androidtree/dist/build/git-annex/4.3
	if [ ! -e tmp/androidtree/dist/setup-config ]; then \
		cd tmp/androidtree && $$HOME/.ghc/$(shell cat standalone/android/abiversion)/arm-linux-androideabi/bin/cabal configure -fAndroid $(ANDROID_FLAGS); \
	fi
	cd tmp/androidtree && $$HOME/.ghc/$(shell cat standalone/android/abiversion)/arm-linux-androideabi/bin/cabal build \
		&& mv dist/build/git-annex/git-annex dist/build/git-annex/4.0/git-annex
	cd tmp/androidtree && $$HOME/.ghc/$(shell cat standalone/android/abiversion)/arm-linux-androideabi/bin/cabal build \
		--ghc-options=-optl-z --ghc-options=-optlnocopyreloc \
		&& mv dist/build/git-annex/git-annex dist/build/git-annex/4.3/git-annex

androidapp:
	$(MAKE) android
	$(MAKE) -C standalone/android

# We bypass cabal, and only run the main ghc --make command for a
# fast development built. Note: Does not rebuild C libraries.
fast: dist/caballog
	@$$(grep 'ghc --make' dist/caballog | head -n 1 | sed -e 's/-package-id [^ ]*//g' -e 's/-hide-all-packages//') -O0
	@ln -sf dist/build/git-annex/git-annex git-annex
	@$(MAKE) tags >/dev/null 2>&1 &

dist/caballog: git-annex.cabal
	$(CABAL) configure -f"-Production" -O0
	$(CABAL) build -v2 | tee $@

# Hardcoded command line to make hdevtools start up and work.
# You will need some memory. It's worth it.
# Note: Don't include WebDAV or Webapp. TH use bloats memory > 500 mb!
# TODO should be possible to derive this from caballog.
hdevtools:
	hdevtools --stop-server || true
	hdevtools check git-annex.hs -g -cpp -g -i -g -idist/build/git-annex/git-annex-tmp -g -i. -g -idist/build/autogen -g -Idist/build/autogen -g -Idist/build/git-annex/git-annex-tmp -g -IUtility -g -DWITH_TESTSUITE -g -DWITH_S3 -g -DWITH_ASSISTANT -g -DWITH_INOTIFY -g -DWITH_DBUS -g -DWITH_PAIRING -g -DWITH_XMPP -g -optP-include -g -optPdist/build/autogen/cabal_macros.h -g -odir -g dist/build/git-annex/git-annex-tmp -g -hidir -g dist/build/git-annex/git-annex-tmp -g -stubdir -g dist/build/git-annex/git-annex-tmp -g -threaded -g -Wall -g -XHaskell98 -g -XPackageImports

distributionupdate:
	ghc --make Build/DistributionUpdate
	./Build/DistributionUpdate

.PHONY: git-annex git-union-merge git-recover-repository tags build-stamp

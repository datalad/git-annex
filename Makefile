all=git-annex mans docs

# set to "./Setup" if you lack a cabal program. Or can be set to "stack"
BUILDER?=cabal
GHC?=ghc

PREFIX?=/usr
SHAREDIR?=share

# Am I typing :make in vim? Do a fast build.
ifdef VIM
all=fast
endif

build: $(all)

Build/SysConfig.hs: Build/TestConfig.hs Build/Configure.hs
	if [ "$(BUILDER)" = ./Setup ]; then ghc --make Setup; fi
	if [ "$(BUILDER)" = stack ]; then \
		$(BUILDER) build $(BUILDEROPTIONS); \
	else \
		$(BUILDER) configure --ghc-options="$(shell Build/collect-ghc-options.sh)"; \
	fi

git-annex: Build/SysConfig.hs
	$(BUILDER) build $(BUILDEROPTIONS)
	if [ "$(BUILDER)" = stack ]; then \
		ln -sf $$(find .stack-work/ -name git-annex -type f | grep build/git-annex/git-annex | tail -n 1) git-annex; \
	else \
		ln -sf dist/build/git-annex/git-annex git-annex; \
	fi
	# Work around https://github.com/haskell/cabal/issues/3524
	# when not linked dynamically to haskell libs
	@if ! ldd git-annex | grep -q libHS; then \
		chrpath -d git-annex || echo "** unable to chrpath git-annex; it will be a little bit slower than necessary"; \
	fi

# These are not built normally.
git-union-merge.1: doc/git-union-merge.mdwn
	./Build/mdwn2man git-union-merge 1 doc/git-union-merge.mdwn > git-union-merge.1
git-union-merge:
	$(GHC) --make -threaded $@

install-mans: mans
	install -d $(DESTDIR)$(PREFIX)/$(SHAREDIR)/man/man1
	install -m 0644 man/*.1 $(DESTDIR)$(PREFIX)/$(SHAREDIR)/man/man1

install-docs: docs install-mans
	install -d $(DESTDIR)$(PREFIX)/$(SHAREDIR)/doc/git-annex
	if [ -d html ]; then \
		rsync -a --delete html/ $(DESTDIR)$(PREFIX)/$(SHAREDIR)/doc/git-annex/html/; \
	fi

install-bins: build
	install -d $(DESTDIR)$(PREFIX)/bin
	install git-annex $(DESTDIR)$(PREFIX)/bin
	ln -sf git-annex $(DESTDIR)$(PREFIX)/bin/git-annex-shell
	ln -sf git-annex $(DESTDIR)$(PREFIX)/bin/git-remote-tor-annex

install-misc: Build/InstallDesktopFile
	./Build/InstallDesktopFile $(PREFIX)/bin/git-annex || true
	install -d $(DESTDIR)$(PREFIX)/share/bash-completion/completions
	install -m 0644 bash-completion.bash $(DESTDIR)$(PREFIX)/share/bash-completion/completions/git-annex

install: install-bins install-docs install-misc

test: git-annex
	./git-annex test

retest: git-annex
	./git-annex test --rerun-update --rerun-filter failures

# hothasktags chokes on some template haskell etc, so ignore errors
tags:
	(for f in $$(find . | grep -v /.git/ | grep -v /tmp/ | grep -v /dist/ | grep -v /doc/ | egrep '\.hs$$'); do hothasktags -c --cpp -c -traditional -c --include=dist/build/autogen/cabal_macros.h $$f; done) 2>/dev/null | sort > tags

# If ikiwiki is available, build static html docs suitable for being
# shipped in the software package.
ifeq ($(shell which ikiwiki),)
IKIWIKI=echo "** ikiwiki not found, skipping building docs" >&2; true
else
IKIWIKI=ikiwiki
endif

mans: Build/MakeMans
	./Build/MakeMans

docs: mans
	LC_ALL=C TZ=UTC $(IKIWIKI) doc html -v --wikiname git-annex \
		--plugin=goodstuff \
		--no-usedirs --disable-plugin=openid --plugin=sidebar \
		--plugin theme --set theme=actiontabs --set deterministic=1 \
		--disable-plugin=shortcut --disable-plugin=smiley \
		--plugin=comments --set comments_pagespec="*" \
		--exclude='ikiwiki/*' \
		--exclude='news/.*' --exclude='design/assistant/blog/*' \
		--exclude='bugs/*' --exclude='todo/*' --exclude='forum/*' \
		--exclude='users/*' --exclude='devblog/*' --exclude='thanks'

clean:
	if [ "$(BUILDER)" != ./Setup ] && [ "$(BUILDER)" != cabal ]; then $(BUILDER) clean; fi
	rm -rf tmp dist git-annex $(mans) configure  *.tix .hpc \
		doc/.ikiwiki html dist tags Build/SysConfig.hs \
		Setup Build/InstallDesktopFile Build/EvilSplicer \
		Build/Standalone Build/OSXMkLibs Build/LinuxMkLibs \
		Build/DistributionUpdate Build/BuildVersion Build/MakeMans \
		git-union-merge .tasty-rerun-log
	find . -name \*.o -exec rm {} \;
	find . -name \*.hi -exec rm {} \;

Build/InstallDesktopFile: Build/InstallDesktopFile.hs
	$(GHC) --make $@ -Wall -fno-warn-tabs
Build/EvilSplicer: Build/EvilSplicer.hs
	$(GHC) --make $@ -Wall -fno-warn-tabs
Build/Standalone: Build/Standalone.hs Build/SysConfig.hs
	$(GHC) --make $@ -Wall -fno-warn-tabs
Build/OSXMkLibs: Build/OSXMkLibs.hs
	$(GHC) --make $@ -Wall -fno-warn-tabs
Build/LinuxMkLibs: Build/LinuxMkLibs.hs
	$(GHC) --make $@ -Wall -fno-warn-tabs
Build/MakeMans: Build/MakeMans.hs
	$(GHC) --make $@ -Wall -fno-warn-tabs

LINUXSTANDALONE_DEST=tmp/git-annex.linux
linuxstandalone:
	$(MAKE) git-annex linuxstandalone-nobuild
linuxstandalone-nobuild: Build/Standalone Build/LinuxMkLibs
	rm -rf "$(LINUXSTANDALONE_DEST)"
	mkdir -p tmp
	cp -R standalone/linux/skel "$(LINUXSTANDALONE_DEST)"
	sed -i -e 's/^GIT_ANNEX_PACKAGE_INSTALL=/GIT_ANNEX_PACKAGE_INSTALL=$(GIT_ANNEX_PACKAGE_INSTALL)/' "$(LINUXSTANDALONE_DEST)/runshell"
	
	install -d "$(LINUXSTANDALONE_DEST)/bin"
	cp git-annex "$(LINUXSTANDALONE_DEST)/bin/"
	strip "$(LINUXSTANDALONE_DEST)/bin/git-annex"
	ln -sf git-annex "$(LINUXSTANDALONE_DEST)/bin/git-annex-shell"
	ln -sf git-annex "$(LINUXSTANDALONE_DEST)/bin/git-remote-tor-annex"
	zcat standalone/licences.gz > $(LINUXSTANDALONE_DEST)/LICENSE
	cp doc/logo_16x16.png doc/logo.svg $(LINUXSTANDALONE_DEST)
	cp standalone/trustedkeys.gpg $(LINUXSTANDALONE_DEST)

	./Build/Standalone "$(LINUXSTANDALONE_DEST)"
	
	install -d "$(LINUXSTANDALONE_DEST)/git-core"
	(cd "$(shell git --exec-path)" && tar c .) | (cd "$(LINUXSTANDALONE_DEST)"/git-core && tar x)
	install -d "$(LINUXSTANDALONE_DEST)/templates"
	install -d "$(LINUXSTANDALONE_DEST)/magic"
	cp /usr/share/file/magic.mgc "$(LINUXSTANDALONE_DEST)/magic"
	cp /usr/share/i18n -a "$(LINUXSTANDALONE_DEST)"
	
	./Build/LinuxMkLibs "$(LINUXSTANDALONE_DEST)"
	
	$(MAKE) install-mans DESTDIR="$(LINUXSTANDALONE_DEST)"

	cd tmp/git-annex.linux && find . -type f > git-annex.MANIFEST
	cd tmp/git-annex.linux && find . -type l >> git-annex.MANIFEST
	cd tmp && tar c git-annex.linux | gzip -9 --rsyncable > git-annex-standalone-$(shell dpkg --print-architecture).tar.gz

# Run this target to build git-annex-standalone*.deb
debianstandalone: dpkg-buildpackage-F
# Run this target to build git-annex-standalone*.dsc
debianstandalone-dsc: dpkg-buildpackage-S

prep-standalone:
	$(MAKE) undo-standalone
	QUILT_PATCHES=debian/patches QUILT_SERIES=series.standalone-build quilt push -a
	debian/create-standalone-changelog

undo-standalone:
	test -e .git
	git checkout debian/changelog CHANGELOG
	quilt pop -a || true

commit-standalone:
	QUILT_PATCHES=debian/patches QUILT_SERIES=series.standalone-build  quilt refresh

dpkg-buildpackage%: prep-standalone
	umask 022; dpkg-buildpackage -rfakeroot $*
	$(MAKE) undo-standalone

OSXAPP_DEST=tmp/build-dmg/git-annex.app
OSXAPP_BASE=$(OSXAPP_DEST)/Contents/MacOS/bundle
osxapp: Build/Standalone Build/OSXMkLibs
	$(MAKE) git-annex

	# Remove all RPATHs, both because this overloads the linker on
	# OSX Sierra, and to avoid the binary looking in someone's home
	# directory.
	eval install_name_tool $$(otool -l git-annex | grep "path " | sed 's/.*path /-delete_rpath /' | sed 's/ (.*//') git-annex

	rm -rf "$(OSXAPP_DEST)" "$(OSXAPP_BASE)"
	install -d tmp/build-dmg
	cp -R standalone/osx/git-annex.app "$(OSXAPP_DEST)"

	install -d "$(OSXAPP_BASE)"
	cp git-annex "$(OSXAPP_BASE)"
	strip "$(OSXAPP_BASE)/git-annex"
	ln -sf git-annex "$(OSXAPP_BASE)/git-annex-shell"
	ln -sf git-annex "$(OSXAPP_BASE)/git-remote-tor-annex"
	gzcat standalone/licences.gz > $(OSXAPP_BASE)/LICENSE
	cp $(OSXAPP_BASE)/LICENSE tmp/build-dmg/LICENSE.txt
	cp standalone/trustedkeys.gpg $(OSXAPP_DEST)/Contents/MacOS

	./Build/Standalone $(OSXAPP_BASE)

	(cd "$(shell git --exec-path)" && tar c .) | (cd "$(OSXAPP_BASE)" && tar x)
	install -d "$(OSXAPP_BASE)/templates"
	install -d "$(OSXAPP_BASE)/magic"
	if [ -e "$(OSX_MAGIC_FILE)" ]; then \
		cp "$(OSX_MAGIC_FILE)" "$(OSXAPP_BASE)/magic/magic.mgc"; \
	else \
		echo "** OSX_MAGIC_FILE not set; not including it" >&2; \
	fi

	# OSX looks in man dir nearby the bin
	$(MAKE) install-mans DESTDIR="$(OSXAPP_BASE)/.." SHAREDIR="" PREFIX=""

	./Build/OSXMkLibs $(OSXAPP_BASE)
	cd $(OSXAPP_DEST) && find . -type f > Contents/MacOS/git-annex.MANIFEST
	cd $(OSXAPP_DEST) && find . -type l >> Contents/MacOS/git-annex.MANIFEST
	rm -f tmp/git-annex.dmg
	hdiutil create -format UDBZ -size 640m -srcfolder tmp/build-dmg \
		-volname git-annex -o tmp/git-annex.dmg

ANDROID_FLAGS?=
# Cross compile for Android.
# Uses https://github.com/neurocyte/ghc-android
android: Build/EvilSplicer
	echo "Running native build, to get TH splices.."
	if [ ! -e dist/setup/setup ]; then $(BUILDER) configure -O0 $(ANDROID_FLAGS) -fAndroidSplice;  fi
	mkdir -p tmp
	if ! $(BUILDER) build --ghc-options=-ddump-splices 2> tmp/dump-splices; then tail tmp/dump-splices >&2; exit 1; fi
	echo "Setting up Android build tree.."
	./Build/EvilSplicer tmp/splices tmp/dump-splices standalone/no-th/evilsplicer-headers.hs
	rsync -az --exclude tmp --exclude dist . tmp/androidtree
# Copy the files with expanded splices to the source tree, but
# only if the existing source file is not newer. (So, if a file
# used to have TH splices but they were removed, it will be newer,
# and not overwritten.)
	cp -uR tmp/splices/* tmp/androidtree || true
# Some additional dependencies needed by the expanded splices.
	sed -i 's/^  Build-Depends: */  Build-Depends: yesod-core, yesod-routes, shakespeare, blaze-markup, file-embed, wai-app-static, wai, unordered-containers, /' tmp/androidtree/git-annex.cabal
# Avoid warnings due to sometimes unused imports added for the splices.
	sed -i 's/GHC-Options: \(.*\)-Wall/GHC-Options: \1-Wall -fno-warn-unused-imports /i' tmp/androidtree/git-annex.cabal
	sed -i 's/Extensions: /Extensions: MagicHash /i' tmp/androidtree/git-annex.cabal
# Cabal cannot cross compile with custom build type, so workaround.
	sed -i 's/Build-type: Custom/Build-type: Simple/' tmp/androidtree/git-annex.cabal
# Build just once, but link repeatedly, for different versions of Android.
	mkdir -p tmp/androidtree/dist/build/git-annex/4.0 tmp/androidtree/dist/build/git-annex/4.3 tmp/androidtree/dist/build/git-annex/5.0
	if [ ! -e tmp/androidtree/dist/setup-config ]; then \
		cd tmp/androidtree && CROSS_COMPILE=Android $$HOME/.ghc/$(shell cat standalone/android/abiversion)/arm-linux-androideabi/bin/cabal configure -fAndroid $(ANDROID_FLAGS); \
	fi
	cd tmp/androidtree && $$HOME/.ghc/$(shell cat standalone/android/abiversion)/arm-linux-androideabi/bin/cabal build \
		&& mv dist/build/git-annex/git-annex dist/build/git-annex/4.0/git-annex
	cd tmp/androidtree && $$HOME/.ghc/$(shell cat standalone/android/abiversion)/arm-linux-androideabi/bin/cabal build \
		--ghc-options=-optl-z --ghc-options=-optlnocopyreloc \
		&& mv dist/build/git-annex/git-annex dist/build/git-annex/4.3/git-annex
	cd tmp/androidtree && $$HOME/.ghc/$(shell cat standalone/android/abiversion)/arm-linux-androideabi/bin/cabal build \
		--ghc-options=-optl-z --ghc-options=-optlnocopyreloc --ghc-options=-optl-fPIE --ghc-options=-optl-pie --ghc-options=-optc-fPIE --ghc-options=-optc-pie \
		&& mv dist/build/git-annex/git-annex dist/build/git-annex/5.0/git-annex

androidapp:
	$(MAKE) android
	$(MAKE) -C standalone/android

# Bypass cabal, and only run the main ghc --make command for a
# faster development build.
fast: dist/cabalbuild
	@sh dist/cabalbuild
	@ln -sf dist/build/git-annex/git-annex git-annex
	@$(MAKE) tags >/dev/null 2>&1 &

dist/cabalbuild: dist/caballog
	grep 'ghc --make' dist/caballog | tail -n 1 > dist/cabalbuild
	
dist/caballog: git-annex.cabal
	$(BUILDER) configure -f"-Production" -O0 --enable-executable-dynamic
	$(BUILDER) build -v2 --ghc-options="-O0 -j" | tee dist/caballog

# Hardcoded command line to make hdevtools start up and work.
# You will need some memory. It's worth it.
# Note: Don't include WebDAV or Webapp. TH use bloats memory > 500 mb!
# TODO should be possible to derive this from caballog.
hdevtools:
	hdevtools --stop-server || true
	hdevtools check git-annex.hs -g -cpp -g -i -g -idist/build/git-annex/git-annex-tmp -g -i. -g -idist/build/autogen -g -Idist/build/autogen -g -Idist/build/git-annex/git-annex-tmp -g -IUtility -g -DWITH_TESTSUITE -g -DWITH_S3 -g -DWITH_ASSISTANT -g -DWITH_INOTIFY -g -DWITH_DBUS -g -DWITH_PAIRING -g -DWITH_XMPP -g -optP-include -g -optPdist/build/autogen/cabal_macros.h -g -odir -g dist/build/git-annex/git-annex-tmp -g -hidir -g dist/build/git-annex/git-annex-tmp -g -stubdir -g dist/build/git-annex/git-annex-tmp -g -threaded -g -Wall -g -XHaskell98 -g -XPackageImports

distributionupdate:
	git pull
	cabal configure
	ghc -Wall -fno-warn-tabs --make Build/DistributionUpdate -XPackageImports -optP-include -optPdist/build/autogen/cabal_macros.h
	./Build/DistributionUpdate

.PHONY: git-annex git-union-merge tags

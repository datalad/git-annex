all=git-annex git-annex-shell mans docs

# set to "./Setup" if you lack a cabal program. Or can be set to "stack"
BUILDER?=cabal
ifeq ($(BUILDER),stack)
GHC?=stack ghc --
else
GHC?=ghc
endif

PREFIX?=/usr
SHAREDIR?=share

# Am I typing :make in vim? Do a fast build.
ifdef VIM
all=fast
endif

build: $(all)

# install system-wide
# Set PREFIX and DESTDIR to configure where it is installed
install: install-bins install-docs install-completions install-desktop

# installs into your home directory
install-home:
	$(MAKE) install-bins PREFIX=$(HOME)/.local
	$(MAKE) install-mans PREFIX=$(HOME)/.local
	$(MAKE) install-desktop PREFIX=$(HOME)/.local USERDIR=1

tmp/configure-stamp: Build/TestConfig.hs Build/Configure.hs
	if [ "$(BUILDER)" = ./Setup ]; then $(GHC) --make Setup; fi
	if [ "$(BUILDER)" != stack ]; then \
		$(BUILDER) configure $(BUILDERCOMMONOPTIONS) --ghc-options="$(shell Build/collect-ghc-options.sh)"; \
	else \
		$(BUILDER) setup $(BUILDERCOMMONOPTIONS); \
	fi
	mkdir -p tmp
	touch tmp/configure-stamp

git-annex: tmp/configure-stamp
	$(BUILDER) build $(BUILDERCOMMONOPTIONS) $(BUILDEROPTIONS)
	if [ "$(BUILDER)" = stack ]; then \
		ln -sf $$(stack path $(BUILDERCOMMONOPTIONS) --dist-dir)/build/git-annex/git-annex git-annex; \
	else \
		ln -sf dist/build/git-annex/git-annex git-annex; \
	fi

git-annex-shell: git-annex
	ln -sf git-annex git-annex-shell

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

install-desktop: build Build/InstallDesktopFile
	./Build/InstallDesktopFile $(PREFIX)/bin/git-annex || true

install-completions:
	install -d $(DESTDIR)$(PREFIX)/$(SHAREDIR)/bash-completion/completions
	install -m 0644 bash-completion.bash $(DESTDIR)$(PREFIX)/$(SHAREDIR)/bash-completion/completions/git-annex
	install -d $(DESTDIR)$(PREFIX)/$(SHAREDIR)/zsh/vendor-completions
	@./git-annex --zsh-completion-script git-annex 2>/dev/null > $(DESTDIR)$(PREFIX)/$(SHAREDIR)/zsh/vendor-completions/_git-annex || \
		echo "** zsh completions not installed; built with too old version of optparse-applicative"
	install -d $(DESTDIR)$(PREFIX)/$(SHAREDIR)/fish/completions
	@./git-annex --fish-completion-script git-annex 2>/dev/null > $(DESTDIR)$(PREFIX)/$(SHAREDIR)/fish/completions/git-annex.fish || \
		echo "** fish completions not installed; built with too old version of optparse-applicative"

test: git-annex git-annex-shell
	./git-annex test

retest: git-annex
	./git-annex test --rerun-update --rerun-filter failures

# hothasktags chokes on some template haskell etc, so ignore errors
tags:
	(for f in $$(find . | grep -v /.git/ | grep -v /tmp/ | grep -v /dist/ | grep -v /doc/ | egrep '\.hs$$'); do hothasktags -XLambdaCase -XPackageImports -c --cpp -c -traditional -c --include=dist/build/git-annex/autogen/cabal_macros.h $$f; done) 2>/dev/null | sort > tags

mans: Build/MakeMans
	./Build/MakeMans

# If ikiwiki is available, build static html docs suitable for being
# shipped in the software package.
docs: mans
	@if [ -n "`which ikiwiki`" ]; then \
		LC_ALL=C TZ=UTC ikiwiki doc html -v --wikiname git-annex \
			--plugin=goodstuff \
			--no-usedirs --disable-plugin=openid --plugin=sidebar \
			--plugin theme --set theme=actiontabs --set deterministic=1 \
			--disable-plugin=shortcut --disable-plugin=smiley \
			--plugin=comments --set comments_pagespec="*" \
			--exclude='ikiwiki/*' \
			--exclude='news/.*' --exclude='design/assistant/blog/*' \
			--exclude='bugs/*' --exclude='todo/*' --exclude='forum/*' \
			--exclude='users/*' --exclude='devblog/*' --exclude='thanks'; \
	else \
		echo "** ikiwiki not found, skipping building docs" >&2; \
	fi

clean:
	if [ "$(BUILDER)" != ./Setup ] && [ "$(BUILDER)" != cabal ]; then $(BUILDER) clean; fi
	rm -rf tmp dist git-annex $(mans) configure  *.tix .hpc \
		doc/.ikiwiki html dist tags Build/SysConfig Build/Version \
		Setup Build/InstallDesktopFile \
		Build/Standalone Build/OSXMkLibs Build/LinuxMkLibs \
		Build/DistributionUpdate Build/BuildVersion Build/MakeMans \
		git-annex-shell git-union-merge .tasty-rerun-log
	find . -name \*.o -exec rm {} \;
	find . -name \*.hi -exec rm {} \;

Build/InstallDesktopFile: Build/InstallDesktopFile.hs
	$(GHC) --make $@ -Wall -fno-warn-tabs
Build/Standalone: Build/Standalone.hs tmp/configure-stamp
	$(GHC) --make $@ -Wall -fno-warn-tabs
Build/BuildVersion: Build/BuildVersion.hs
	$(GHC) --make $@ -Wall -fno-warn-tabs
Build/OSXMkLibs: Build/OSXMkLibs.hs
	$(GHC) --make $@ -Wall -fno-warn-tabs
Build/LinuxMkLibs: Build/LinuxMkLibs.hs
	$(GHC) --make $@ -Wall -fno-warn-tabs
Build/MakeMans: Build/MakeMans.hs
	$(GHC) --make $@ -Wall -fno-warn-tabs

LINUXSTANDALONE_DEST=tmp/git-annex.linux
linuxstandalone: 
	$(MAKE) git-annex Build/Standalone Build/LinuxMkLibs
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
	(cd "$(shell git --man-path)"/../git-core/templates && tar c .) | (cd "$(LINUXSTANDALONE_DEST)"/templates && tar x)
	install -d "$(LINUXSTANDALONE_DEST)/magic"
	cp /usr/share/file/magic.mgc "$(LINUXSTANDALONE_DEST)/magic"
	cp /usr/share/i18n -a "$(LINUXSTANDALONE_DEST)"
	
	./Build/LinuxMkLibs "$(LINUXSTANDALONE_DEST)"
	
	$(MAKE) install-mans DESTDIR="$(LINUXSTANDALONE_DEST)"

	sha1sum git-annex > "$(LINUXSTANDALONE_DEST)/buildid"
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
osxapp:
	$(MAKE) git-annex Build/Standalone Build/OSXMkLibs Build/BuildVersion

	# Remove all RPATHs, both because this overloads the linker on
	# OSX Sierra, and to avoid the binary looking in someone's home
	# directory.
	if otool -l git-annex | grep -q "path "; then \
		eval install_name_tool $$(otool -l git-annex | grep "path " | sed 's/.*path /-delete_rpath /' | sed 's/ (.*//') git-annex; \
	fi

	rm -rf "$(OSXAPP_DEST)" "$(OSXAPP_BASE)"
	install -d tmp/build-dmg
	cp -R standalone/osx/git-annex.app "$(OSXAPP_DEST)"
	sed -e 's/GIT_ANNEX_VERSION/$(shell Build/BuildVersion)/' \
		< standalone/osx/Info.plist.template \
		> "$(OSXAPP_DEST)"/Contents/Info.plist

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
	(cd "$(shell git --man-path)"/../git-core/templates && tar c .) | (cd "$(OSXAPP_BASE)"/templates && tar x)
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
	hdevtools check git-annex.hs -g -cpp -g -i -g -idist/build/git-annex/git-annex-tmp -g -i. -g -idist/build/autogen -g -Idist/build/autogen -g -Idist/build/git-annex/git-annex-tmp -g -IUtility -g -DWITH_S3 -g -DWITH_ASSISTANT -g -DWITH_INOTIFY -g -DWITH_DBUS -g -DWITH_PAIRING -g -g -optP-include -g -optPdist/build/autogen/cabal_macros.h -g -odir -g dist/build/git-annex/git-annex-tmp -g -hidir -g dist/build/git-annex/git-annex-tmp -g -stubdir -g dist/build/git-annex/git-annex-tmp -g -threaded -g -Wall -g -XHaskell98 -g -XPackageImports -g -XLambdaCase

distributionupdate:
	git pull
	cabal configure
	ghc -Wall -fno-warn-tabs --make Build/DistributionUpdate -XLambdaCase -XPackageImports
	./Build/DistributionUpdate

.PHONY: git-annex git-union-merge tags

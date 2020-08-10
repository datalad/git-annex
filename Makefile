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

# On Debian systems, zsh does not look here, and you will have to override
# this to /usr/share/zsh/vendor-completions
ZSH_COMPLETIONS_PATH?=$(PREFIX)/$(SHAREDIR)/zsh/site-functions

# Am I typing :make in vim? Do a dev build.
ifdef VIM
all=dev
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

# Non-optimised build for development.
#
# This leaves cabal.project.local configured for a dev build,
# so just running make will continue to do dev builds.
dev:
	$(BUILDER) configure -f"-Production" -O0 --enable-executable-dynamic
	mkdir -p tmp
	touch tmp/configure-stamp
	$(MAKE) git-annex

git-annex: tmp/configure-stamp
	$(BUILDER) build $(BUILDERCOMMONOPTIONS) $(BUILDEROPTIONS)
	@if [ "$(BUILDER)" = stack ]; then \
		ln -sf $$(stack path $(BUILDERCOMMONOPTIONS) --dist-dir)/build/git-annex/git-annex git-annex; \
	else \
		if [ -d dist-newstyle ]; then \
			ln -sf $$(cabal exec -- sh -c 'command -v git-annex') git-annex; \
		else \
			ln -sf dist/build/git-annex/git-annex git-annex; \
		fi; \
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

install-completions: build
	install -d $(DESTDIR)$(PREFIX)/$(SHAREDIR)/bash-completion/completions
	install -m 0644 bash-completion.bash $(DESTDIR)$(PREFIX)/$(SHAREDIR)/bash-completion/completions/git-annex
	install -d $(DESTDIR)$(ZSH_COMPLETIONS_PATH)
	./git-annex --zsh-completion-script git-annex 2>/dev/null \
		> $(DESTDIR)$(ZSH_COMPLETIONS_PATH)/_git-annex
	install -d $(DESTDIR)$(PREFIX)/$(SHAREDIR)/fish/vendor_completions.d
	./git-annex --fish-completion-script git-annex 2>/dev/null \
		> $(DESTDIR)$(PREFIX)/$(SHAREDIR)/fish/vendor_completions.d/git-annex.fish

test: git-annex git-annex-shell
	./git-annex test

retest: git-annex
	./git-annex test --rerun-update --rerun-filter failures

# tags file for vim
tags:
	@$(MAKE) --quiet hothasktags HOTHASKTAGS_OPT= TAGFILE=tags

# TAGS file for emacs
TAGS:
	@$(MAKE) --quiet hothasktags HOTHASKTAGS_OPT=-e TAGFILE=TAGS

# https://github.com/luqui/hothasktags/issues/18
HOTHASKTAGS_ARGS=-XLambdaCase -XPackageImports --cpp

hothasktags:
	@if ! cabal exec hothasktags -- $(HOTHASKTAGS_OPT) $(HOTHASKTAGS_ARGS) \
		$$(find . | grep -v /.git/ | grep -v /tmp/ | grep -v dist/ | grep -v /doc/ | egrep '\.hs$$') 2>/dev/null \
			| sort > $(TAGFILE); then \
		echo "** hothasktags failed"; \
	fi

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
	rm -rf tmp dist dist-newstyle git-annex $(mans) configure  *.tix .hpc \
		doc/.ikiwiki html dist tags TAGS Build/SysConfig Build/Version \
		Setup Build/InstallDesktopFile Build/Standalone \
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
Build/MakeMans: Build/MakeMans.hs
	$(GHC) --make $@ -Wall -fno-warn-tabs

LINUXSTANDALONE_DEST=tmp/git-annex.linux
linuxstandalone:
	$(MAKE) git-annex Build/Standalone
	./Build/Standalone "$(LINUXSTANDALONE_DEST)" "$(LINUXSTANDALONE_DEST)"

	$(MAKE) install-mans DESTDIR="$(LINUXSTANDALONE_DEST)"
	zcat standalone/licences.gz > $(LINUXSTANDALONE_DEST)/LICENSE
	cp doc/logo_16x16.png doc/logo.svg $(LINUXSTANDALONE_DEST)
	cp standalone/trustedkeys.gpg $(LINUXSTANDALONE_DEST)

	sha1sum git-annex > "$(LINUXSTANDALONE_DEST)/buildid"
	cd tmp/git-annex.linux && find . -type f > git-annex.MANIFEST
	cd tmp/git-annex.linux && find . -type l >> git-annex.MANIFEST
	cd tmp && tar c git-annex.linux | gzip -9 --rsyncable > git-annex-standalone-$(shell dpkg --print-architecture).tar.gz

# Run this target to build git-annex-standalone.deb
debianstandalone: dpkg-buildpackage-F
# Run this target to build git-annex-standalone.dsc
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
OSXAPP_TOP=$(OSXAPP_DEST)/Contents/MacOS/bundle
osxapp:
	$(MAKE) git-annex
	# Remove all RPATHs, both because this overloads the linker on
	# OSX Sierra, and to avoid the binary looking in someone's home
	# directory.
	if otool -l git-annex | grep -q "path "; then \
		eval install_name_tool $$(otool -l git-annex | grep "path " | sed 's/.*path /-delete_rpath /' | sed 's/ (.*//') git-annex; \
	fi

	$(MAKE) Build/Standalone
	./Build/Standalone $(OSXAPP_TOP) $(OSXAPP_DEST)

	gzcat standalone/licences.gz > $(OSXAPP_TOP)/LICENSE
	cp $(OSXAPP_TOP)/LICENSE tmp/build-dmg/LICENSE.txt
	cp standalone/trustedkeys.gpg $(OSXAPP_DEST)/Contents/MacOS

	# OSX looks in man dir nearby the bin
	$(MAKE) install-mans DESTDIR="$(OSXAPP_TOP)/.." SHAREDIR="" PREFIX=""

	cd $(OSXAPP_DEST) && find . -type f > Contents/MacOS/git-annex.MANIFEST
	cd $(OSXAPP_DEST) && find . -type l >> Contents/MacOS/git-annex.MANIFEST
	rm -f tmp/git-annex.dmg

	# hdiutil sometimes fails with "resource busy", so try a few times
	ok=0; for x in 1 2 3 4 5; do \
		if [ $$ok = 0 ]; then \
			if hdiutil create -format UDBZ -size 640m -srcfolder tmp/build-dmg \
				-volname git-annex -o tmp/git-annex.dmg; \
			then \
				ok=1; \
			else \
				sleep 60; \
			fi \
		fi \
	done; if [ $$ok = 0 ]; then exit 1; fi

distributionupdate:
	git pull
	cabal configure
	ghc -Wall -fno-warn-tabs --make Build/DistributionUpdate -XLambdaCase -XPackageImports
	./Build/DistributionUpdate

.PHONY: git-annex git-union-merge tags TAGS

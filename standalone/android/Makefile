# Cross-compiles utilities needed for git-annex on Android,
# and builds the Android app.

# Add Android cross-compiler to PATH (as installed by ghc-android)
# (This directory also needs to have a cc that is a symlink to the prefixed
# gcc cross-compiler executable.)
ANDROID_CROSS_COMPILER?=$(HOME)/.ghc/android-14/arm-linux-androideabi-4.7/bin
PATH:=$(ANDROID_CROSS_COMPILER):$(PATH)

# Paths to the Android SDK and NDK.
export ANDROID_SDK_ROOT?=$(HOME)/tmp/adt-bundle-linux-x86/sdk
export ANDROID_NDK_ROOT?=$(HOME)/tmp/android-ndk-r8d

GITTREE=source/git/installed-tree

build: build-utils start
	# Debug build because it does not need signing keys.
	cd source/term && tools/build-debug

	# Install executables as pseudo-libraries so they will be
	# unpacked from the .apk.
	mkdir -p source/term/libs/armeabi
	cp ../../git-annex source/term/libs/armeabi/lib.git-annex.so
	cp source/busybox/busybox source/term/libs/armeabi/lib.busybox.so
	cp source/openssh/ssh source/term/libs/armeabi/lib.ssh.so
	cp source/openssh/ssh-keygen source/term/libs/armeabi/lib.ssh-keygen.so
	cp source/rsync/rsync source/term/libs/armeabi/lib.rsync.so
	cp source/gnupg/g10/gpg source/term/libs/armeabi/lib.gpg.so
	cp source/git/git source/term/libs/armeabi/lib.git.so
	cp source/git/git-shell source/term/libs/armeabi/lib.git-shell.so
	cp source/git/git-upload-pack source/term/libs/armeabi/lib.git-upload-pack.so
	arm-linux-androideabi-strip --strip-unneeded --remove-section=.comment --remove-section=.note source/term/libs/armeabi/*
	cp runshell source/term/libs/armeabi/lib.runshell.so
	cp start source/term/libs/armeabi/lib.start.so
	
	# remove git stuff we don't need to save space
	rm -rf $(GITTREE)/bin/git-cvsserver \
		$(GITTREE)/libexec/git-core/git-daemon \
		$(GITTREE)/libexec/git-core/git-show-index \
		$(GITTREE)/libexec/git-core/mergetools \
		$(GITTREE)/libexec/git-core/git-credential-* \
		$(GITTREE)/libexec/git-core/git-cvsserver \
		$(GITTREE)/libexec/git-core/git-cvsimport \
		$(GITTREE)/libexec/git-core/git-fast-import \
		$(GITTREE)/libexec/git-core/git-http-backend \
		$(GITTREE)/libexec/git-core/git-imap-send \
		$(GITTREE)/libexec/git-core/git-instaweb \
		$(GITTREE)/libexec/git-core/git-p4 \
		$(GITTREE)/libexec/git-core/git-remote-test* \
		$(GITTREE)/libexec/git-core/git-submodule \
		$(GITTREE)/libexec/git-core/git-svn \
		$(GITTREE)/libexec/git-core/git-web--browse
	# Most of git is in one multicall binary, but a few important
	# commands are still shell scripts. Those are put into
	# a tarball, along with a list of all the hard links that should be
	# set up.
	cd $(GITTREE) && mkdir -p links
	cd $(GITTREE) && find -samefile bin/git -not -wholename ./bin/git > links/git
	cd $(GITTREE) && find -samefile bin/git-shell -not -wholename ./bin/git-shell > links/git-shell
	cd $(GITTREE) && find -samefile bin/git-upload-pack -not -wholename ./bin/git-upload-pack > links/git-upload-pack
	cd $(GITTREE) && find -type f -not -samefile bin/git -not -samefile bin/git-shell -not -samefile bin/git-upload-pack|tar czf ../git.tar.gz -T -
	cp source/git/git.tar.gz source/term/libs/armeabi/lib.git.tar.gz.so

	git rev-parse HEAD > source/term/libs/armeabi/lib.version.so

	cd source/term && ant debug

build-utils: source
	$(MAKE) build-openssh
	$(MAKE) build-busybox
	$(MAKE) build-rsync
	$(MAKE) build-gnupg
	$(MAKE) build-git
	$(MAKE) build-term
	touch build-utils

build-openssh:
	cd source/openssl && CC=$$(which cc) ./Configure android
	cd source/openssl && $(MAKE)

	cd source/openssh && git reset --hard
	cd source/openssh && ./configure --host=arm-linux-androideabi --with-ssl-dir=../openssl --without-openssl-header-check
	cd source/openssh && patch -p1 < ../../openssh.patch
	cp openssh.config.h source/openssh/config.h
	cd source/openssh && sed -i -e 's/getrrsetbyname.o //' openbsd-compat/Makefile
	cd source/openssh && sed -i -e 's/auth-passwd.o //' Makefile
	cd source/openssh && $(MAKE) ssh ssh-keygen

build-busybox:
	cp busybox_config source/busybox/.config
	cd source/busybox && yes '' | $(MAKE) oldconfig
	cd source/busybox && $(MAKE)
	
build-git:
	cd source/git && $(MAKE) install NO_OPENSSL=1 NO_GETTEXT=1 NO_GECOS_IN_PWENT=1 NO_GETPASS=1 NO_NSEC=1 NO_MKDTEMP=1 NO_PTHREADS=1 NO_PERL=1 NO_CURL=1 NO_EXPAT=1 NO_TCLTK=1 NO_ICONV=1 prefix= DESTDIR=installed-tree

build-rsync:
	cd source/rsync && git reset --hard origin/master && git am < ../../rsync.patch
	cp source/automake/lib/config.sub source/automake/lib/config.guess source/rsync/
	cd source/rsync && ./configure --host=arm-linux-androideabi --disable-locale --disable-iconv-open --disable-iconv --disable-acl-support --disable-xattr-support
	cd source/rsync && $(MAKE)

build-gnupg:
	cd source/gnupg && git checkout gnupg-1.4.13
	cd source/gnupg && ./autogen.sh
	cd source/gnupg && ./configure --host=arm-linux-androideabi --disable-gnupg-iconv --enable-minimal --disable-card-support --disable-agent-support --disable-photo-viewers --disable-keyserver-helpers --disable-nls
	cd source/gnupg; $(MAKE) || true # expected failure in doc build

build-term:
	cd source/term && git reset --hard
	cd source/term && patch -p1 <../../term.patch
	(cd icons && tar c .) | (cd source/term/res && tar x)
	# This renaming has a purpose. It makes the path to the app's
	# /data directory shorter, which makes ssh connection caching
	# sockets placed there have more space for their filenames.
	# Also, it avoids overlap with the Android Terminal Emulator
	# app, if it's also installed.
	cd source/term && find -name .git -prune -o -type f -print0 | xargs -0 perl -pi -e 's/jackpal/ga/g'
	cd source/term && perl -pi -e 's/Terminal Emulator/Git Annex/g' res/*/strings.xml
	cd source/term && tools/update.sh

source: clones
	mkdir -p source
	cd source && \
	for p in automake busybox git rsync gnupg openssl openssh term; do \
		git clone ../clones/$$p $$p; \
	done

clones:
	mkdir -p clones
	git clone --bare git://git.savannah.gnu.org/automake.git clones/automake
	git clone --bare git://git.debian.org/git/d-i/busybox clones/busybox
	git clone --bare git://git.kernel.org/pub/scm/git/git.git clones/git
	git clone --bare git://git.samba.org/rsync.git clones/rsync
	git clone --bare git://git.gnupg.org/gnupg.git clones/gnupg
	git clone --bare git://git.openssl.org/openssl clones/openssl
	git clone --bare git://github.com/CyanogenMod/android_external_openssh.git clones/openssh
	git clone --bare git://github.com/jackpal/Android-Terminal-Emulator.git clones/term

clean:
	rm -rf $(GITTREE) source
	rm -f build-utils start

reallyclean: clean
	rm -rf clones

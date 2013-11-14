# Cross-compiles utilities needed for git-annex on Android,
# and builds the Android app.

# Add Android cross-compiler to PATH (as installed by ghc-android)
ANDROID_CROSS_COMPILER?=$(HOME)/.ghc/$(shell cat abiversion)/bin
PATH:=$(ANDROID_CROSS_COMPILER):$(PATH)

# Paths to the Android SDK and NDK.
export ANDROID_SDK_ROOT?=$(HOME)/.android/adt-bundle-linux-x86/sdk
export ANDROID_NDK_ROOT?=$(HOME)/.android/android-ndk

# Where to store the source tree used to build utilities. This
# directory will be created by `make source`.
GIT_ANNEX_ANDROID_SOURCETREE?=$(HOME)/.android/git-annex-sourcetree

GITTREE=$(GIT_ANNEX_ANDROID_SOURCETREE)/git/installed-tree

build: start
	if [ ! -e "$(GIT_ANNEX_ANDROID_SOURCETREE)" ]; then $(MAKE) source; fi
	$(MAKE) $(GIT_ANNEX_ANDROID_SOURCETREE)/openssl/build-stamp
	$(MAKE) $(GIT_ANNEX_ANDROID_SOURCETREE)/openssh/build-stamp
	$(MAKE) $(GIT_ANNEX_ANDROID_SOURCETREE)/busybox/build-stamp
	$(MAKE) $(GIT_ANNEX_ANDROID_SOURCETREE)/rsync/build-stamp
	$(MAKE) $(GIT_ANNEX_ANDROID_SOURCETREE)/gnupg/build-stamp
	$(MAKE) $(GIT_ANNEX_ANDROID_SOURCETREE)/git/build-stamp
	$(MAKE) $(GIT_ANNEX_ANDROID_SOURCETREE)/term/build-stamp

	# Debug build because it does not need signing keys.
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/term && tools/build-debug

	# Install executables as pseudo-libraries so they will be
	# unpacked from the .apk.
	mkdir -p $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi
	cp $(GIT_ANNEX_ANDROID_SOURCETREE)/busybox/busybox $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi/lib.busybox.so
	cp $(GIT_ANNEX_ANDROID_SOURCETREE)/openssh/ssh $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi/lib.ssh.so
	cp $(GIT_ANNEX_ANDROID_SOURCETREE)/openssh/ssh-keygen $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi/lib.ssh-keygen.so
	cp $(GIT_ANNEX_ANDROID_SOURCETREE)/rsync/rsync $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi/lib.rsync.so
	cp $(GIT_ANNEX_ANDROID_SOURCETREE)/gnupg/g10/gpg $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi/lib.gpg.so
	cp $(GIT_ANNEX_ANDROID_SOURCETREE)/git/git $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi/lib.git.so
	cp $(GIT_ANNEX_ANDROID_SOURCETREE)/git/git-shell $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi/lib.git-shell.so
	cp $(GIT_ANNEX_ANDROID_SOURCETREE)/git/git-upload-pack $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi/lib.git-upload-pack.so
	arm-linux-androideabi-strip --strip-unneeded --remove-section=.comment --remove-section=.note $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi/*
	cp runshell $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi/lib.runshell.so
	cp start $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi/lib.start.so
	
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
	# a tarball, along with a list of all the links that should be
	# set up.
	cd $(GITTREE) && mkdir -p links
	cd $(GITTREE) && find -samefile bin/git -not -wholename ./bin/git > links/git
	cd $(GITTREE) && find -samefile bin/git-shell -not -wholename ./bin/git-shell > links/git-shell
	cd $(GITTREE) && find -samefile bin/git-upload-pack -not -wholename ./bin/git-upload-pack > links/git-upload-pack
	cd $(GITTREE) && find -type f -not -samefile bin/git -not -samefile bin/git-shell -not -samefile bin/git-upload-pack|tar czf ../git.tar.gz -T -
	cp $(GIT_ANNEX_ANDROID_SOURCETREE)/git/git.tar.gz $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi/lib.git.tar.gz.so

	git rev-parse HEAD > $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi/lib.version.so

	mkdir -p ../../tmp/4.0 ../../tmp/4.3
	
	cp ../../tmp/androidtree/dist/build/git-annex/4.3/git-annex $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi/lib.git-annex.so
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/term && ant debug
	cp $(GIT_ANNEX_ANDROID_SOURCETREE)/term/bin/Term-debug.apk ../../tmp/4.3/git-annex.apk
	
	cp ../../tmp/androidtree/dist/build/git-annex/4.0/git-annex $(GIT_ANNEX_ANDROID_SOURCETREE)/term/libs/armeabi/lib.git-annex.so
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/term && ant debug
	cp $(GIT_ANNEX_ANDROID_SOURCETREE)/term/bin/Term-debug.apk ../../tmp/4.0/git-annex.apk

$(GIT_ANNEX_ANDROID_SOURCETREE)/openssl/build-stamp:
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/openssl && CC=$$(which cc) ./Configure android
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/openssl && $(MAKE)
	touch $@

$(GIT_ANNEX_ANDROID_SOURCETREE)/openssh/build-stamp: openssh.patch openssh.config.h
	# This is a known-good version that the patch works with.
	# TODO: Upgrade
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/openssh && git reset --hard 0a8617ed5af2f0248d0e9648e26b224e16ada742
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/openssh && ./configure --host=arm-linux-androideabi --with-ssl-dir=../openssl --without-openssl-header-check
	cat openssh.patch | (cd $(GIT_ANNEX_ANDROID_SOURCETREE)/openssh && patch -p1)
	cp openssh.config.h $(GIT_ANNEX_ANDROID_SOURCETREE)/openssh/config.h
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/openssh && sed -i -e 's/getrrsetbyname.o //' openbsd-compat/Makefile
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/openssh && sed -i -e 's/auth-passwd.o //' Makefile
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/openssh && $(MAKE) ssh ssh-keygen
	touch $@

$(GIT_ANNEX_ANDROID_SOURCETREE)/busybox/build-stamp: busybox_config
	cp busybox_config $(GIT_ANNEX_ANDROID_SOURCETREE)/busybox/.config
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/busybox && yes '' | $(MAKE) oldconfig
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/busybox && $(MAKE)
	touch $@
	
$(GIT_ANNEX_ANDROID_SOURCETREE)/git/build-stamp:
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/git && $(MAKE) install NO_OPENSSL=1 NO_GETTEXT=1 NO_GECOS_IN_PWENT=1 NO_GETPASS=1 NO_NSEC=1 NO_MKDTEMP=1 NO_PTHREADS=1 NO_PERL=1 NO_CURL=1 NO_EXPAT=1 NO_TCLTK=1 NO_ICONV=1 prefix= DESTDIR=installed-tree
	touch $@

$(GIT_ANNEX_ANDROID_SOURCETREE)/rsync/build-stamp: rsync.patch
	# This is a known-good version that the patch works with.
	cat rsync.patch | (cd $(GIT_ANNEX_ANDROID_SOURCETREE)/rsync && git reset --hard eec26089b1c7bdbb260674480ffe6ece257bca63 && git am)
	cp $(GIT_ANNEX_ANDROID_SOURCETREE)/automake/lib/config.sub $(GIT_ANNEX_ANDROID_SOURCETREE)/automake/lib/config.guess $(GIT_ANNEX_ANDROID_SOURCETREE)/rsync/
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/rsync && ./configure --host=arm-linux-androideabi --disable-locale --disable-iconv-open --disable-iconv --disable-acl-support --disable-xattr-support
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/rsync && $(MAKE)
	touch $@

$(GIT_ANNEX_ANDROID_SOURCETREE)/gnupg/build-stamp:
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/gnupg && git checkout gnupg-1.4.15
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/gnupg && ./autogen.sh
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/gnupg && ./configure --host=arm-linux-androideabi --disable-gnupg-iconv --disable-card-support --disable-agent-support --disable-photo-viewers --disable-keyserver-helpers --disable-nls
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/gnupg; $(MAKE) || true # expected failure in doc build
	touch $@

$(GIT_ANNEX_ANDROID_SOURCETREE)/term/build-stamp: term.patch icons
	# This is a known-good version that the patch works with.
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/term && git reset --hard 3d34b3c42295c215b62e70f3ee696dd664ba08ce
	cat term.patch | (cd $(GIT_ANNEX_ANDROID_SOURCETREE)/term && patch -p1)
	(cd icons && tar c .) | (cd $(GIT_ANNEX_ANDROID_SOURCETREE)/term/res && tar x)
	# This renaming has a purpose. It makes the path to the app's
	# /data directory shorter, which makes ssh connection caching
	# sockets placed there have more space for their filenames.
	# Also, it avoids overlap with the Android Terminal Emulator
	# app, if it's also installed.
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/term && find -name .git -prune -o -type f -print0 | xargs -0 perl -pi -e 's/jackpal/ga/g'
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/term && perl -pi -e 's/Terminal Emulator/Git Annex/g' res/*/strings.xml
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/term && echo y | tools/update.sh
	touch $@

source: $(GIT_ANNEX_ANDROID_SOURCETREE)

$(GIT_ANNEX_ANDROID_SOURCETREE):
	mkdir -p $(GIT_ANNEX_ANDROID_SOURCETREE)
	git clone git://git.savannah.gnu.org/automake.git $(GIT_ANNEX_ANDROID_SOURCETREE)/automake
	git clone git://git.debian.org/git/d-i/busybox $(GIT_ANNEX_ANDROID_SOURCETREE)/busybox
	git clone git://git.kernel.org/pub/scm/git/git.git $(GIT_ANNEX_ANDROID_SOURCETREE)/git
	git clone git://git.samba.org/rsync.git $(GIT_ANNEX_ANDROID_SOURCETREE)/rsync
	git clone git://git.gnupg.org/gnupg.git $(GIT_ANNEX_ANDROID_SOURCETREE)/gnupg
	git clone git://git.openssl.org/openssl $(GIT_ANNEX_ANDROID_SOURCETREE)/openssl
	git clone git://github.com/CyanogenMod/android_external_openssh.git $(GIT_ANNEX_ANDROID_SOURCETREE)/openssh
	git clone git://github.com/jackpal/Android-Terminal-Emulator.git $(GIT_ANNEX_ANDROID_SOURCETREE)/term

clean:
	rm -rf $(GITTREE)
	rm -f start

reallyclean: clean
	rm -rf $(GIT_ANNEX_ANDROID_SOURCETREE)

# Cross-compiles utilities needed for git-annex on Android,
# and builds the Android app.

build:
	./buildapk 4
	./buildapk 5

# Targets below are used by buildapk, which sets
# GIT_ANNEX_ANDROID_SOURCETREE

source: $(GIT_ANNEX_ANDROID_SOURCETREE)

$(GIT_ANNEX_ANDROID_SOURCETREE):
	mkdir -p $(GIT_ANNEX_ANDROID_SOURCETREE)
	git clone git://git.debian.org/git/d-i/busybox $(GIT_ANNEX_ANDROID_SOURCETREE)/busybox
	git clone git://git.kernel.org/pub/scm/git/git.git $(GIT_ANNEX_ANDROID_SOURCETREE)/git
	git clone git://git.samba.org/rsync.git $(GIT_ANNEX_ANDROID_SOURCETREE)/rsync
	git clone git://git.gnupg.org/gnupg.git $(GIT_ANNEX_ANDROID_SOURCETREE)/gnupg
	git clone git://git.openssl.org/openssl $(GIT_ANNEX_ANDROID_SOURCETREE)/openssl
	git clone git://github.com/CyanogenMod/android_external_openssh.git $(GIT_ANNEX_ANDROID_SOURCETREE)/openssh
	git clone git://github.com/jackpal/Android-Terminal-Emulator.git $(GIT_ANNEX_ANDROID_SOURCETREE)/term

$(GIT_ANNEX_ANDROID_SOURCETREE)/openssl/build-stamp:
	# This is a version which the openssh below can build with.
	# Newer versions changed something to do with BIGNUM.
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/openssl && git reset --hard 616f71e486d693991b594439c884ec624b32c2d4
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
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/busybox && git reset --hard a758e3e1e04e7705f5d37b2f27be654cd0e7282c
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/busybox && yes '' | $(MAKE) oldconfig
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/busybox && $(MAKE)
	touch $@
	
$(GIT_ANNEX_ANDROID_SOURCETREE)/git/build-stamp: git.patch
	# This is a known-good version that the patch works with.
	cat git.patch | (cd $(GIT_ANNEX_ANDROID_SOURCETREE)/git && git reset --hard d9c691a759d62cef53a6cc11864a2ef4b0829244 && git am)
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/git && $(MAKE) install NO_OPENSSL=1 NO_GETTEXT=1 NO_GECOS_IN_PWENT=1 NO_GETPASS=1 NO_NSEC=1 NO_MKDTEMP=1 NO_PTHREADS=1 NO_PERL=1 NO_CURL=1 NO_EXPAT=1 NO_TCLTK=1 NO_ICONV=1 HAVE_CLOCK_GETTIME= HAVE_GETDELIM= prefix= DESTDIR=installed-tree
	touch $@

$(GIT_ANNEX_ANDROID_SOURCETREE)/rsync/build-stamp: rsync.patch
	# This is a known-good version that the patch works with.
	cat rsync.patch | (cd $(GIT_ANNEX_ANDROID_SOURCETREE)/rsync && git reset --hard eec26089b1c7bdbb260674480ffe6ece257bca63 && git am)
	cp /usr/share/misc/config.sub /usr/share/misc/config.guess $(GIT_ANNEX_ANDROID_SOURCETREE)/rsync/
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
	cd $(GIT_ANNEX_ANDROID_SOURCETREE)/term && echo y | tools/update.sh || true
	touch $@

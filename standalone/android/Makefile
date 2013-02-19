# Cross-compiles utilities needed for git-annex on Android.

# Add Android cross-compiler to PATH (as installed by ghc-android)
# (This directory also needs to have a cc that is a symlink to the prefixed
# gcc cross-compiler executable.)
PATH:=$(HOME)/.ghc/android-14/arm-linux-androideabi-4.7/bin:$(PATH)

build: source
	mkdir -p git-annex-bundle/bin

	cd source/openssl && CC=$$(which cc) ./Configure android
	cd source/openssl && $(MAKE)

	cd source/openssh && git reset --hard
	cd source/openssh && ./configure --host=arm-linux-androideabi --with-ssl-dir=../openssl --without-openssl-header-check
	cd source/openssh && patch -p1 < ../../openssh.patch
	cp openssh.config.h source/openssh/config.h
	cd source/openssh && sed -i -e 's/getrrsetbyname.o //' openbsd-compat/Makefile
	cd source/openssh && sed -i -e 's/auth-passwd.o //' Makefile
	cd source/openssh && $(MAKE) ssh ssh-keygen
	cp -a source/openssh/ssh source/openssh/ssh-keygen git-annex-bundle/bin/

	cp busybox_config source/busybox/.config
	cd source/busybox && yes '' | $(MAKE) oldconfig
	cd source/busybox && $(MAKE)
	cp -a source/busybox/busybox git-annex-bundle/bin/
	
	cd source/git && $(MAKE) install NO_OPENSSL=1 NO_GETTEXT=1 NO_GECOS_IN_PWENT=1 NO_GETPASS=1 NO_NSEC=1 NO_MKDTEMP=1 NO_PTHREADS=1 NO_PERL=1 NO_CURL=1 NO_EXPAT=1 NO_TCLTK=1 NO_ICONV=1 prefix= DESTDIR=../../git-annex-bundle
	rm -f git-annex-bundle/bin/git-cvsserver

	cd source/rsync && git reset --hard origin/master && git am < ../../rsync.patch
	cp source/automake/lib/config.sub source/automake/lib/config.guess source/rsync/
	cd source/rsync && ./configure --host=arm-linux-androideabi --disable-locale --disable-iconv-open --disable-iconv --disable-acl-support --disable-xattr-support
	cd source/rsync && $(MAKE)
	cp -a source/rsync/rsync git-annex-bundle/bin/

	cd source/gnupg && git checkout gnupg-1.4.13
	cd source/gnupg && ./autogen.sh
	cd source/gnupg && ./configure --host=arm-linux-androideabi --disable-gnupg-iconv --enable-minimal --disable-card-support --disable-agent-support --disable-photo-viewers --disable-keyserver-helpers --disable-nls
	cd source/gnupg; $(MAKE) || true # expected failure in doc build
	cp -a source/gnupg/g10/gpg git-annex-bundle/bin/

source:
	mkdir -p source
	git clone git://git.savannah.gnu.org/automake.git source/automake
	git clone git://git.debian.org/git/d-i/busybox source/busybox
	git clone git://git.kernel.org/pub/scm/git/git.git source/git
	git clone git://git.samba.org/rsync.git source/rsync
	git clone git://git.gnupg.org/gnupg.git source/gnupg
	git clone git://git.openssl.org/openssl source/openssl
	git clone git://github.com/CyanogenMod/android_external_openssh.git source/openssh

clean:
	cd source/busybox && $(MAKE) clean
	cd source/openssl && $(MAKE) clean
	cd source/openssh && $(MAKE) clean
	cd source/git && $(MAKE) clean
	cd source/rsync && $(MAKE) clean
	cd source/gnupg && $(MAKE) clean

reallyclean:
	rm -rf source

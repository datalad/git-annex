# Cross-compiles utilities needed for git-annex on Android.

# Add Android cross-compiler to PATH (as installed by ghc-android)
PATH:=$(HOME)/.ghc/android-14/arm-linux-androideabi-4.7/bin:$(PATH)

build: source
	mkdir -p git-annex-bundle/bin
	
	cd source/git && $(MAKE) install NO_OPENSSL=1 NO_GETTEXT=1 NO_GECOS_IN_PWENT=1 NO_GETPASS=1 NO_NSEC=1 NO_MKDTEMP=1 NO_PTHREADS=1 NO_PERL=1 NO_CURL=1 NO_EXPAT=1 NO_TCLTK=1 NO_ICONV=1 prefix= DESTDIR=../../git-annex-bundle

	cp source/busybox/configs/android2_defconfig source/busybox/.config
	echo 'CONFIG_FEATURE_INSTALLER=y' >> source/busybox/.config
	cd source/busybox && yes '' | $(MAKE) oldconfig
	cd source/busybox && $(MAKE)
	cp -a source/busybox/busybox git-annex-bundle/bin/

	cd source/dropbear && git reset --hard origin/master && git am < ../../dropbear.patch
	cp source/automake/lib/config.sub source/automake/lib/config.guess source/dropbear/
	cd source/dropbear && ./configure --host=arm-linux-androideabi --disable-lastlog --disable-utmp --disable-utmpx --disable-wtmp --disable-wtmpx
	cd source/dropbear && $(MAKE) dbclient
	cp -a source/dropbear/dbclient git-annex-bundle/bin/ssh
	
	cd source/rsync && git reset --hard origin/master && git am < ../../rsync.patch
	cp source/automake/lib/config.sub source/automake/lib/config.guess source/rsync/
	cd source/rsync && ./configure --host=arm-linux-androideabi --disable-locale --disable-iconv-open --disable-iconv --disable-acl-support --disable-xattr-support
	cd source/rsync && $(MAKE)
	cp -a source/rsync/rsync git-annex-bundle/bin/

	cd source/gnupg && scripts/autogen.sh
	cd source/gnupg && ./configure --host=arm-linux-androideabi --disable-gnupg-iconv --enable-minimal --disable-card-support --disable-agent-support --disable-photo-viewers --disable-keyserver-helpers --disable-nls
	cd source/gnupg; $(MAKE) || true # expected failure in doc build
	cp -a source/gnupg/g10/gpg git-annex-bundle/bin/

source:
	mkdir -p source
	git clone git://git.savannah.gnu.org/automake.git source/automake
	git clone git://git.debian.org/git/d-i/busybox source/busybox
	git clone git://github.com/android/platform_external_dropbear source/dropbear
	git clone git://git.kernel.org/pub/scm/git/git.git source/git
	git clone git://git.samba.org/rsync.git source/rsync
	git clone git://git.gnupg.org/gnupg.git source/gnupg

clean:
	cd source/busybox && $(MAKE) clean
	cd source/dropbear && $(MAKE) clean
	cd source/git && $(MAKE) clean
	cd source/rsync && $(MAKE) clean
	cd source/gnupg && $(MAKE) clean

reallyclean:
	rm -rf source

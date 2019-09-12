Name: git-annex-standalone
Version: %{version}
Release: %{release}
Summary: manage files with git, without checking their contents into git
License: AGPL
AutoReqProv: no
Requires: git

%description
git-annex allows managing files with git, without checking the file
contents into git. While that may seem paradoxical, it is useful when
dealing with files larger than git can currently easily handle, whether due
to limitations in memory, time, or disk space.

It can store large files in many places, from local hard drives, to a
large number of cloud storage services, including S3, WebDAV,
and rsync, with a dozen cloud storage providers usable via plugins.
Files can be stored encrypted with gpg, so that the cloud storage
provider cannot see your data. git-annex keeps track of where each file
is stored, so it knows how many copies are available, and has many
facilities to ensure your data is preserved.

git-annex can also be used to keep a folder in sync between computers,
noticing when files are changed, and automatically committing them
to git and transferring them to other computers. The git-annex webapp
makes it easy to set up and use git-annex this way.

%build
# Need to have run make linuxstandalone (or untarred the standalone
# tarball) before building this rpm; the git-annex source code is not built
# here. --build-root has to be pointed at the git-annex.linux directory.

%install
mkdir -p %{buildroot}/usr/lib/
cp -a %{buildroot}/../git-annex.linux %{buildroot}/usr/lib
mkdir -p %{buildroot}/usr/bin/
ln -sf /usr/lib/git-annex.linux/git-annex %{buildroot}/usr/bin/git-annex
ln -sf /usr/lib/git-annex.linux/git-annex %{buildroot}/usr/bin/git-annex-shell

%files
%attr(-, root, root)
/usr/bin/git-annex
/usr/bin/git-annex-shell
/usr/lib/git-annex.linux

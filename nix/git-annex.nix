{ mkDerivation, aeson, async, attoparsec, base, bloomfilter, bup
, byteable, bytestring, Cabal, case-insensitive, concurrent-output
, conduit, connection, containers, crypto-api, cryptonite, curl
, data-default, DAV, dbus, directory, disk-free-space, dlist
, edit-distance, exceptions, fdo-notify, feed, filepath, free, git
, gnupg, hinotify, hslogger, http-client, http-client-tls
, http-conduit, http-types, IfElse, lsof, magic, memory, microlens
, monad-control, monad-logger, mountpoints, mtl, network
, network-info, network-multicast, network-uri, old-locale, openssh
, optparse-applicative, perl, persistent, persistent-sqlite
, persistent-template, process, QuickCheck, random, regex-tdfa
, resourcet, rsync, SafeSemaphore, sandi, securemem, socks, split
, stdenv, stm, stm-chans, tagsoup, tasty, tasty-hunit
, tasty-quickcheck, tasty-rerun, text, time, torrent, transformers
, unix, unix-compat, unordered-containers, utf8-string, uuid
, vector, wget, which
}:
mkDerivation {
  pname = "git-annex";
  version = "7.20181031";
  src = ./..;
  configureFlags = [
    "-fassistant" "-fcryptonite" "-fdbus" "-fdesktopnotify" "-fdns"
    "-ffeed" "-finotify" "-fpairing" "-fproduction" "-fquvi" "-f-s3"
    "-ftahoe" "-ftdfa" "-ftestsuite" "-ftorrentparser" "-f-webapp"
    "-f-webapp-secure" "-fwebdav" "-fxmpp"
  ];
  isLibrary = false;
  isExecutable = true;
  setupHaskellDepends = [
    base bytestring Cabal data-default directory exceptions filepath
    hslogger IfElse process split transformers unix-compat utf8-string
  ];
  executableHaskellDepends = [
    aeson async attoparsec base bloomfilter byteable bytestring
    case-insensitive concurrent-output conduit connection containers
    crypto-api cryptonite data-default DAV dbus directory
    disk-free-space dlist edit-distance exceptions fdo-notify feed
    filepath free hinotify hslogger http-client http-client-tls
    http-conduit http-types IfElse magic memory microlens monad-control
    monad-logger mountpoints mtl network network-info network-multicast
    network-uri old-locale optparse-applicative persistent
    persistent-sqlite persistent-template process QuickCheck random
    regex-tdfa resourcet SafeSemaphore sandi securemem socks split stm
    stm-chans tagsoup tasty tasty-hunit tasty-quickcheck tasty-rerun
    text time torrent transformers unix unix-compat
    unordered-containers utf8-string uuid vector
  ];
  executableSystemDepends = [
    bup curl git gnupg lsof openssh perl rsync wget which
  ];
  preConfigure = "export HOME=$TEMPDIR; patchShebangs .";
  installPhase = "make PREFIX=$out BUILDER=: install";
  checkPhase = ''
    ln -sf dist/build/git-annex/git-annex git-annex
    ln -sf git-annex git-annex-shell
    export PATH+=":$PWD"
    git-annex test
  '';
  enableSharedExecutables = false;
  homepage = "http://git-annex.branchable.com/";
  description = "manage files with git, without checking their contents into git";
  license = stdenv.lib.licenses.gpl3;
}

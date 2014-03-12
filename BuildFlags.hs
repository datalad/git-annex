{- git-annex build flags reporting
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module BuildFlags where

buildFlags :: [String]
buildFlags = filter (not . null)
	[ ""
#ifdef WITH_ASSISTANT
	, "Assistant"
#endif
#ifdef WITH_WEBAPP
	, "Webapp"
#endif
#ifdef WITH_WEBAPP_HTTPS
	, "Webapp-https"
#endif
#ifdef WITH_PAIRING
	, "Pairing"
#endif
#ifdef WITH_TESTSUITE
	, "Testsuite"
#endif
#ifdef WITH_S3
	, "S3"
#endif
#ifdef WITH_WEBDAV
	, "WebDAV"
#endif
#ifdef WITH_INOTIFY
	, "Inotify"
#endif
#ifdef WITH_FSEVENTS
	, "FsEvents"
#endif
#ifdef WITH_KQUEUE
	, "Kqueue"
#endif
#ifdef WITH_DBUS
	, "DBus"
#endif
#ifdef WITH_XMPP
	, "XMPP"
#endif
#ifdef WITH_DNS
	, "DNS"
#endif
#ifdef WITH_FEED
	, "Feeds"
#endif
#ifdef WITH_QUVI
	, "Quvi"
#endif
#ifdef WITH_TDFA
	, "TDFA"
#endif
#ifdef WITH_CRYPTOHASH
	, "CryptoHash"
#endif
#ifdef WITH_EKG
	, "EKG"
#endif
	]

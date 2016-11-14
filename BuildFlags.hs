{- git-annex build flags reporting
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
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
#else
#warning Building without the assistant.
#endif
#ifdef WITH_WEBAPP
	, "Webapp"
#else
#warning Building without the webapp. You probably need to install Yesod..
#endif
#ifdef WITH_PAIRING
	, "Pairing"
#else
#warning Building without local pairing.
#endif
#ifdef WITH_TESTSUITE
	, "Testsuite"
#else
#warning Building without the testsuite.
#endif
#ifdef WITH_S3
	, "S3"
#if MIN_VERSION_aws(0,10,6)
		++ "(multipartupload)"
#endif
#if MIN_VERSION_aws(0,13,0)
		++ "(storageclasses)"
#endif
#else
#warning Building without S3.
#endif
#ifdef WITH_WEBDAV
	, "WebDAV"
#else
#warning Building without WebDAV.
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
#ifdef WITH_DESKTOP_NOTIFY
	, "DesktopNotify"
#endif
#ifdef WITH_CONCURRENTOUTPUT
	, "ConcurrentOutput"
#else
#warning Building without ConcurrentOutput
#endif
#ifdef WITH_TORRENTPARSER
	, "TorrentParser"
#endif
#ifdef WITH_MAGICMIME
	, "MagicMime"
#endif
	-- Always enabled now, but users may be used to seeing these flags
	-- listed.
	, "Feeds"
	, "Quvi"
	]

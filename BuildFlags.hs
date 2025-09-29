{- git-annex build flags
 -
 - Copyright 2013-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module BuildFlags where

import Data.List
import Data.Ord
import qualified Data.CaseInsensitive as CI

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
#warning Building without the webapp.
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
#ifdef WITH_TORRENTPARSER
	, "TorrentParser"
#endif
#ifdef WITH_MAGICMIME
	, "MagicMime"
#endif
#ifdef WITH_BENCHMARK
	, "Benchmark"
#endif
#ifdef DEBUGLOCKS
	, "DebugLocks"
#endif
	-- Always enabled now, but users may be used to seeing these flags
	-- listed.
	, "Feeds"
	, "Testsuite"
	, "S3"
	, "WebDAV"
	, "Servant"
#ifdef WITH_OSPATH
	, "OsPath"
#else 
#warning Building without the OsPath build flag set results in slower filename manipulation and is not recommended.
#endif
	]

-- Not a complete list, let alone a listing transitive deps, but only
-- the ones that are often interesting to know.
dependencyVersions :: [String]
dependencyVersions = map fmt $ sortBy (comparing (CI.mk . fst))
	[ ("feed", VERSION_feed)
	, ("uuid", VERSION_uuid)
	, ("bloomfilter", VERSION_bloomfilter)
	, ("http-client", VERSION_http_client)
	, ("crypton", VERSION_crypton)
	, ("aws", VERSION_aws)
	, ("DAV", VERSION_DAV)
#ifdef WITH_TORRENTPARSER
	, ("torrent", VERSION_torrent)
#endif
#ifdef WITH_WEBAPP
	, ("yesod", VERSION_yesod)
#endif
#ifdef TOOL_VERSION_ghc
	, ("ghc", TOOL_VERSION_ghc)
#endif
	]
  where
	fmt (p, v) = p ++ "-" ++ v

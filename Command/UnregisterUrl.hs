{- git-annex command
 -
 - Copyright 2015-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Command.UnregisterUrl where

import Command
import Logs.Web
import Command.RegisterUrl (seekBatch, start, optParser, RegisterUrlOptions(..))

cmd :: Command
cmd = withAnnexOptions [jsonOptions] $ command "unregisterurl"
	SectionPlumbing "unregisters an url for a key"
	(paramPair paramKey paramUrl)
	(seek <$$> optParser)

seek :: RegisterUrlOptions -> CommandSeek
seek o = case (batchOption o, keyUrlPairs o) of
	(Batch fmt, _) -> seekBatch unregisterUrl o fmt
	(NoBatch, ps) -> commandAction (start unregisterUrl o ps)

unregisterUrl :: Remote -> Key -> String -> Annex ()
unregisterUrl _remote key url = do
	-- Remove the url no matter what downloader;
	-- registerurl can set OtherDownloader, and this should also
	-- be able to remove urls added by addurl, which may use
	-- YoutubeDownloader.
	forM_ [minBound..maxBound] $ \dl ->
		setUrlMissing key (setDownloader url dl)
	-- Unlike unregisterurl, this does not update location tracking
	-- for remotes other than the web special remote. Doing so with
	-- a remote that git-annex can drop content from would rather
	-- unexpectedly leave content stranded on that remote.

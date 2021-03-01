{- git-annex command
 -
 - Copyright 2015-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Command.UnregisterUrl where

import Command
import Logs.Web
import Command.RegisterUrl (start, startMass, optParser, RegisterUrlOptions(..))

cmd :: Command
cmd = command "unregisterurl"
	SectionPlumbing "unregisters an url for a key"
	(paramPair paramKey paramUrl)
	(seek <$$> optParser)

seek :: RegisterUrlOptions -> CommandSeek
seek o = case (batchOption o, keyUrlPairs o) of
	(Batch fmt, _) -> commandAction $ startMass setUrlMissing fmt
	(NoBatch, ps) -> withWords (commandAction . start setUrlMissing) ps

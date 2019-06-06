{- git-annex command
 -
 - Copyright 2015-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Command.RegisterUrl where

import Command
import Logs.Web
import Command.FromKey (mkKey)
import qualified Remote

cmd :: Command
cmd = notDirect $ notBareRepo $
	command "registerurl"
		SectionPlumbing "registers an url for a key"
		(paramPair paramKey paramUrl)
		(seek <$$> optParser)

data RegisterUrlOptions = RegisterUrlOptions
	{ keyUrlPairs :: CmdParams
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser RegisterUrlOptions
optParser desc = RegisterUrlOptions
	<$> cmdParams desc
	<*> parseBatchOption

seek :: RegisterUrlOptions -> CommandSeek
seek o = case (batchOption o, keyUrlPairs o) of
	(Batch fmt, _) -> commandAction $ startMass fmt
	-- older way of enabling batch input, does not support BatchNull
	(NoBatch, []) -> commandAction $ startMass BatchLine
	(NoBatch, ps) -> withWords (commandAction . start) ps

start :: [String] -> CommandStart
start (keyname:url:[]) = 
	starting "registerurl" (ActionItemOther (Just url)) $ do
		let key = mkKey keyname
		perform key url
start _ = giveup "specify a key and an url"

startMass :: BatchFormat -> CommandStart
startMass fmt = 
	starting "registerurl" (ActionItemOther (Just "stdin")) $
		massAdd fmt

massAdd :: BatchFormat -> CommandPerform
massAdd fmt = go True =<< map (separate (== ' ')) <$> batchLines fmt
  where
	go status [] = next $ return status
	go status ((keyname,u):rest) | not (null keyname) && not (null u) = do
		let key = mkKey keyname
		ok <- perform' key u
		let !status' = status && ok
		go status' rest
	go _ _ = giveup "Expected pairs of key and url on stdin, but got something else."

perform :: Key -> URLString -> CommandPerform
perform key url = do
	ok <- perform' key url
	next $ return ok

perform' :: Key -> URLString -> Annex Bool
perform' key url = do
	r <- Remote.claimingUrl url
	setUrlPresent key (setDownloader' url r)
	return True

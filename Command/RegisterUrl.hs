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
import Command.FromKey (keyOpt)
import qualified Remote

cmd :: Command
cmd = command "registerurl"
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
	(Batch fmt, _) -> commandAction $ startMass setUrlPresent fmt
	-- older way of enabling batch input, does not support BatchNull
	(NoBatch, []) -> commandAction $ startMass setUrlPresent BatchLine
	(NoBatch, ps) -> withWords (commandAction . start setUrlPresent) ps

start :: (Key -> URLString -> Annex ()) -> [String] -> CommandStart
start a (keyname:url:[]) = 
	starting "registerurl" ai si $
		perform a (keyOpt keyname) url
  where
	ai = ActionItemOther (Just url)
	si = SeekInput [keyname, url]
start _ _ = giveup "specify a key and an url"

startMass :: (Key -> URLString -> Annex ()) -> BatchFormat -> CommandStart
startMass a fmt = 
	starting "registerurl" (ActionItemOther (Just "stdin")) (SeekInput []) $
		performMass a fmt

performMass :: (Key -> URLString -> Annex ()) -> BatchFormat -> CommandPerform
performMass a fmt = go True =<< map (separate (== ' ')) <$> batchLines fmt
  where
	go status [] = next $ return status
	go status ((keyname,u):rest) | not (null keyname) && not (null u) = do
		let key = keyOpt keyname
		ok <- perform' a key u
		let !status' = status && ok
		go status' rest
	go _ _ = giveup "Expected pairs of key and url on stdin, but got something else."

perform :: (Key -> URLString -> Annex ()) -> Key -> URLString -> CommandPerform
perform a key url = do
	ok <- perform' a key url
	next $ return ok

perform' :: (Key -> URLString -> Annex ()) -> Key -> URLString -> Annex Bool
perform' a key url = do
	r <- Remote.claimingUrl url
	a key (setDownloader' url r)
	return True

{- git-annex command
 -
 - Copyright 2010-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Command.FromKey where

import Command
import qualified Annex.Queue
import Annex.Content
import qualified Annex
import qualified Backend.URL

import Network.URI

cmd :: Command
cmd = notDirect $ notBareRepo $
	command "fromkey" SectionPlumbing "adds a file using a specific key"
		(paramRepeating (paramPair paramKey paramPath))
		(seek <$$> optParser)

data FromKeyOptions = FromKeyOptions
	{ keyFilePairs :: CmdParams 
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser FromKeyOptions
optParser desc = FromKeyOptions
	<$> cmdParams desc
	<*> parseBatchOption

seek :: FromKeyOptions -> CommandSeek
seek o = case (batchOption o, keyFilePairs o) of
	(Batch fmt, _) -> commandAction $ startMass fmt
	-- older way of enabling batch input, does not support BatchNull
	(NoBatch, []) -> commandAction $ startMass BatchLine
	(NoBatch, ps) -> do
		force <- Annex.getState Annex.force
		withPairs (commandAction . start force) ps

start :: Bool -> (String, FilePath) -> CommandStart
start force (keyname, file) = do
	let key = mkKey keyname
	unless force $ do
		inbackend <- inAnnex key
		unless inbackend $ giveup $
			"key ("++ keyname ++") is not present in backend (use --force to override this sanity check)"
	showStart "fromkey" file
	next $ perform key file

startMass :: BatchFormat -> CommandStart
startMass fmt = do
	showStart' "fromkey" (Just "stdin")
	next (massAdd fmt)

massAdd :: BatchFormat -> CommandPerform
massAdd fmt = go True =<< map (separate (== ' ')) <$> batchLines fmt
  where
	go status [] = next $ return status
	go status ((keyname,f):rest) | not (null keyname) && not (null f) = do
		let key = mkKey keyname
		ok <- perform' key f
		let !status' = status && ok
		go status' rest
	go _ _ = giveup "Expected pairs of key and file on stdin, but got something else."

-- From user input to a Key.
-- User can input either a serialized key, or an url.
--
-- In some cases, an input can be parsed as both a key and as an uri.
-- For example, "WORM--a:a" parses as an uri. To disambiguate, check
-- the uri scheme, to see if it looks like the prefix of a key. This relies
-- on key backend names never containing a ':'.
mkKey :: String -> Key
mkKey s = case parseURI s of
	Just u | not (isKeyPrefix (uriScheme u)) ->
		Backend.URL.fromUrl s Nothing
	_ -> case file2key s of
		Just k -> k
		Nothing -> giveup $ "bad key/url " ++ s

perform :: Key -> FilePath -> CommandPerform
perform key file = do
	ok <- perform' key file
	next $ return ok

perform' :: Key -> FilePath -> Annex Bool
perform' key file = do
	link <- calcRepo $ gitAnnexLink file key
	liftIO $ createDirectoryIfMissing True (parentDir file)
	liftIO $ createSymbolicLink link file
	Annex.Queue.addCommand "add" [Param "--"] [file]
	return True

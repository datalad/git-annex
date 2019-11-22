{- git-annex command
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Command.FromKey where

import Command
import qualified Annex.Queue
import Annex.Content
import Annex.WorkTree
import qualified Annex
import qualified Backend.URL

import Network.URI

cmd :: Command
cmd = notBareRepo $ withGlobalOptions [jsonOptions] $
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
	(Batch fmt, _) -> seekBatch fmt
	-- older way of enabling batch input, does not support BatchNull
	(NoBatch, []) -> seekBatch BatchLine
	(NoBatch, ps) -> do
		force <- Annex.getState Annex.force
		withPairs (commandAction . start force) ps

seekBatch :: BatchFormat -> CommandSeek
seekBatch fmt = batchInput fmt parse commandAction
  where
	parse s = 
		let (keyname, file) = separate (== ' ') s
		in if not (null keyname) && not (null file)
			then Right $ go file (keyOpt keyname)
			else Left "Expected pairs of key and filename"
	go file key = starting "fromkey" (mkActionItem (key, file)) $
		perform key file

start :: Bool -> (String, FilePath) -> CommandStart
start force (keyname, file) = do
	let key = keyOpt keyname
	unless force $ do
		inbackend <- inAnnex key
		unless inbackend $ giveup $
			"key ("++ keyname ++") is not present in backend (use --force to override this sanity check)"
	starting "fromkey" (mkActionItem (key, file)) $
		perform key file

-- From user input to a Key.
-- User can input either a serialized key, or an url.
--
-- In some cases, an input can be parsed as both a key and as an uri.
-- For example, "WORM--a:a" parses as an uri. To disambiguate, check
-- the uri scheme, to see if it looks like the prefix of a key. This relies
-- on key backend names never containing a ':'.
keyOpt :: String -> Key
keyOpt s = case parseURI s of
	Just u | not (isKeyPrefix (uriScheme u)) ->
		Backend.URL.fromUrl s Nothing
	_ -> case deserializeKey s of
		Just k -> k
		Nothing -> giveup $ "bad key/url " ++ s

perform :: Key -> FilePath -> CommandPerform
perform key file = lookupFileNotHidden file >>= \case
	Nothing -> ifM (liftIO $ doesFileExist file)
		( hasothercontent
		, do
			link <- calcRepo $ gitAnnexLink file key
			liftIO $ createDirectoryIfMissing True (parentDir file)
			liftIO $ createSymbolicLink link file
			Annex.Queue.addCommand "add" [Param "--"] [file]
			next $ return True
		)
	Just k
		| k == key -> next $ return True
		| otherwise -> hasothercontent
  where
	hasothercontent = do
		warning $ file ++ " already exists with different content"
		next $ return False

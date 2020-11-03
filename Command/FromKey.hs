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
import Annex.Perms
import qualified Annex
import qualified Backend.URL
import qualified Utility.RawFilePath as R

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
seekBatch fmt = batchInput fmt parse (commandAction . go)
  where
	parse s = do
		let (keyname, file) = separate (== ' ') s
		if not (null keyname) && not (null file)
			then do
				file' <- liftIO $ relPathCwdToFile (toRawFilePath file)
				return $ Right (file', keyOpt keyname)
			else return $
				Left "Expected pairs of key and filename"
	go (si, (file, key)) = 
		let ai = mkActionItem (key, file)
		in starting "fromkey" ai si $
			perform key file

start :: Bool -> (SeekInput, (String, FilePath)) -> CommandStart
start force (si, (keyname, file)) = do
	let key = keyOpt keyname
	unless force $ do
		inbackend <- inAnnex key
		unless inbackend $ giveup $
			"key ("++ keyname ++") is not present in backend (use --force to override this sanity check)"
	let ai = mkActionItem (key, file')
	starting "fromkey" ai si $
		perform key file'
  where
	file' = toRawFilePath file

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

perform :: Key -> RawFilePath -> CommandPerform
perform key file = lookupKeyNotHidden file >>= \case
	Nothing -> ifM (liftIO $ doesFileExist (fromRawFilePath file))
		( hasothercontent
		, do
			link <- calcRepo $ gitAnnexLink file key
			createWorkTreeDirectory (parentDir file)
			liftIO $ R.createSymbolicLink link file
			Annex.Queue.addCommand "add" [Param "--"] [fromRawFilePath file]
			next $ return True
		)
	Just k
		| k == key -> next $ return True
		| otherwise -> hasothercontent
  where
	hasothercontent = do
		warning $ fromRawFilePath file ++ " already exists with different content"
		next $ return False

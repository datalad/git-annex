{- git-annex command
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.FromKey where

import Command
import qualified Annex
import qualified Database.Keys
import qualified Backend.URL
import Annex.Content
import Annex.WorkTree
import Annex.Perms
import Annex.Link
import Annex.FileMatcher
import Annex.Ingest
import Git.FilePath
import Utility.Url.Parse

import Network.URI

cmd :: Command
cmd = notBareRepo $ withAnnexOptions [jsonOptions] $
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
	<*> parseBatchOption False

seek :: FromKeyOptions -> CommandSeek
seek o = do
	matcher <- addUnlockedMatcher
	case (batchOption o, keyFilePairs o) of
		(Batch fmt, _) -> batchOnly Nothing (keyFilePairs o) $
			seekBatch matcher fmt
		-- older way of enabling batch input, does not support BatchNull
		(NoBatch, []) -> seekBatch matcher (BatchFormat BatchLine (BatchKeys False))
		(NoBatch, ps) -> do
			force <- Annex.getRead Annex.force
			withPairs (commandAction . start matcher force) ps

seekBatch :: AddUnlockedMatcher -> BatchFormat -> CommandSeek
seekBatch matcher fmt = batchInput fmt parse (commandAction . go)
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
			perform matcher key file

start :: AddUnlockedMatcher -> Bool -> (SeekInput, (String, FilePath)) -> CommandStart
start matcher force (si, (keyname, file)) = do
	let key = keyOpt keyname
	unless force $ do
		inbackend <- inAnnex key
		unless inbackend $ giveup $
			"key ("++ keyname ++") is not present in backend (use --force to override this sanity check)"
	let ai = mkActionItem (key, file')
	starting "fromkey" ai si $
		perform matcher key file'
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
keyOpt = either giveup id . keyOpt'

keyOpt' :: String -> Either String Key
keyOpt' s = case parseURIPortable s of
	Just u | not (isKeyPrefix (uriScheme u)) ->
		Right $ Backend.URL.fromUrl s Nothing False
	_ -> case deserializeKey s of
		Just k -> Right k
		Nothing -> Left $ "bad key/url " ++ s

perform :: AddUnlockedMatcher -> Key -> RawFilePath -> CommandPerform
perform matcher key file = lookupKeyNotHidden file >>= \case
	Nothing -> ifM (liftIO $ doesFileExist (fromRawFilePath file))
		( hasothercontent
		, do
			contentpresent <- inAnnex key
			objectloc <- calcRepo (gitAnnexLocation key)
			let mi = if contentpresent
				then MatchingFile $ FileInfo
					{ contentFile = objectloc
					, matchFile = file
					, matchKey = Just key
					}
				else keyMatchInfoWithoutContent key file
			createWorkTreeDirectory (parentDir file)
			ifM (addUnlocked matcher mi contentpresent)
				( do
					stagePointerFile file Nothing =<< hashPointerFile key
					Database.Keys.addAssociatedFile key =<< inRepo (toTopFilePath file)
					if contentpresent
						then linkunlocked
						else writepointer
				, do
					link <- calcRepo $ gitAnnexLink file key
					addAnnexLink link file
				)
			next $ return True
		)
	Just k
		| k == key -> next $ return True
		| otherwise -> hasothercontent
  where
	hasothercontent = do
		warning $ QuotedPath file <> " already exists with different content"
		next $ return False
	
	linkunlocked = linkFromAnnex key file Nothing >>= \case
		LinkAnnexFailed -> writepointer
		_ -> return ()
	
	writepointer = liftIO $ writePointerFile file key Nothing

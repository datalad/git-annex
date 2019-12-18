{- git-annex command
 -
 - Copyright 2012-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.ReKey where

import Command
import qualified Annex
import Annex.Content
import Annex.Ingest
import Annex.Link
import Annex.Perms
import Annex.ReplaceFile
import Logs.Location
import Git.FilePath
import qualified Database.Keys
import Annex.InodeSentinal
import Utility.InodeCache
import qualified Utility.RawFilePath as R

cmd :: Command
cmd = command "rekey" SectionPlumbing
	"change keys used for files"
	(paramRepeating $ paramPair paramPath paramKey)
	(seek <$$> optParser)

data ReKeyOptions = ReKeyOptions
	{ reKeyThese :: CmdParams
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser ReKeyOptions
optParser desc = ReKeyOptions
	<$> cmdParams desc
	<*> parseBatchOption

-- Split on the last space, since a FilePath can contain whitespace,
-- but a Key very rarely does.
batchParser :: String -> Either String (RawFilePath, Key)
batchParser s = case separate (== ' ') (reverse s) of
	(rk, rf)
		| null rk || null rf -> Left "Expected: \"file key\""
		| otherwise -> case deserializeKey (reverse rk) of
			Nothing -> Left "bad key"
			Just k -> Right (toRawFilePath (reverse rf), k)

seek :: ReKeyOptions -> CommandSeek
seek o = case batchOption o of
	Batch fmt -> batchInput fmt batchParser (batchCommandAction . start)
	NoBatch -> withPairs (commandAction . start . parsekey) (reKeyThese o)
  where
	parsekey (file, skey) =
		(toRawFilePath file, fromMaybe (giveup "bad key") (deserializeKey skey))

start :: (RawFilePath, Key) -> CommandStart
start (file, newkey) = ifAnnexed file go stop
  where
	go oldkey
		| oldkey == newkey = stop
		| otherwise = starting "rekey" (ActionItemWorkTreeFile file) $
			perform file oldkey newkey

perform :: RawFilePath -> Key -> Key -> CommandPerform
perform file oldkey newkey = do
	ifM (inAnnex oldkey) 
		( unlessM (linkKey file oldkey newkey) $
			giveup "failed creating link from old to new key"
		, unlessM (Annex.getState Annex.force) $
			giveup $ fromRawFilePath file ++ " is not available (use --force to override)"
		)
	next $ cleanup file oldkey newkey

{- Make a hard link to the old key content (when supported),
 - to avoid wasting disk space. -}
linkKey :: RawFilePath -> Key -> Key -> Annex Bool
linkKey file oldkey newkey = ifM (isJust <$> isAnnexLink file)
 	{- If the object file is already hardlinked to elsewhere, a hard
	 - link won't be made by getViaTmpFromDisk, but a copy instead.
	 - This avoids hard linking to content linked to an
	 - unlocked file, which would leave the new key unlocked
	 - and vulnerable to corruption. -}
	( getViaTmpFromDisk RetrievalAllKeysSecure DefaultVerify newkey $ \tmp -> unVerified $ do
		oldobj <- fromRawFilePath <$> calcRepo (gitAnnexLocation oldkey)
		isJust <$> linkOrCopy' (return True) newkey oldobj tmp Nothing
	, do
	 	{- The file being rekeyed is itself an unlocked file; if
		 - it's hard linked to the old key, that link must be broken. -}
		oldobj <- fromRawFilePath <$> calcRepo (gitAnnexLocation oldkey)
		v <- tryNonAsync $ do
			st <- liftIO $ R.getFileStatus file
			when (linkCount st > 1) $ do
				freezeContent oldobj
				replaceFile (fromRawFilePath file) $ \tmp -> do
					unlessM (checkedCopyFile oldkey oldobj tmp Nothing) $
						error "can't lock old key"
					thawContent tmp
		ic <- withTSDelta (liftIO . genInodeCache file)
		case v of
			Left e -> do
				warning (show e)
				return False
			Right () -> do
				r <- linkToAnnex newkey (fromRawFilePath file) ic
				return $ case r of
					LinkAnnexFailed -> False
					LinkAnnexOk -> True
					LinkAnnexNoop -> True
	)

cleanup :: RawFilePath -> Key -> Key -> CommandCleanup
cleanup file oldkey newkey = do
	ifM (isJust <$> isAnnexLink file)
		( do
			-- Update symlink to use the new key.
			liftIO $ removeFile (fromRawFilePath file)
			addLink (fromRawFilePath file) newkey Nothing
		, do
			mode <- liftIO $ catchMaybeIO $ fileMode <$> R.getFileStatus file
			liftIO $ whenM (isJust <$> isPointerFile file) $
				writePointerFile file newkey mode
			stagePointerFile file mode =<< hashPointerFile newkey
			Database.Keys.removeAssociatedFile oldkey 
				=<< inRepo (toTopFilePath file)
		)
	whenM (inAnnex newkey) $
		logStatus newkey InfoPresent
	return True

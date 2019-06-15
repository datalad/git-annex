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

cmd :: Command
cmd = notDirect $ 
	command "rekey" SectionPlumbing
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
batchParser :: String -> Either String (FilePath, Key)
batchParser s = case separate (== ' ') (reverse s) of
	(rk, rf)
		| null rk || null rf -> Left "Expected: \"file key\""
		| otherwise -> case deserializeKey (reverse rk) of
			Nothing -> Left "bad key"
			Just k -> Right (reverse rf, k)

seek :: ReKeyOptions -> CommandSeek
seek o = case batchOption o of
	Batch fmt -> batchInput fmt batchParser (batchCommandAction . start)
	NoBatch -> withPairs (commandAction . start . parsekey) (reKeyThese o)
  where
	parsekey (file, skey) =
		(file, fromMaybe (giveup "bad key") (deserializeKey skey))

start :: (FilePath, Key) -> CommandStart
start (file, newkey) = ifAnnexed file go stop
  where
	go oldkey
		| oldkey == newkey = stop
		| otherwise = starting "rekey" (ActionItemWorkTreeFile file) $
			perform file oldkey newkey

perform :: FilePath -> Key -> Key -> CommandPerform
perform file oldkey newkey = do
	ifM (inAnnex oldkey) 
		( unlessM (linkKey file oldkey newkey) $
			giveup "failed creating link from old to new key"
		, unlessM (Annex.getState Annex.force) $
			giveup $ file ++ " is not available (use --force to override)"
		)
	next $ cleanup file oldkey newkey

{- Make a hard link to the old key content (when supported),
 - to avoid wasting disk space. -}
linkKey :: FilePath -> Key -> Key -> Annex Bool
linkKey file oldkey newkey = ifM (isJust <$> isAnnexLink file)
 	{- If the object file is already hardlinked to elsewhere, a hard
	 - link won't be made by getViaTmpFromDisk, but a copy instead.
	 - This avoids hard linking to content linked to an
	 - unlocked file, which would leave the new key unlocked
	 - and vulnerable to corruption. -}
	( getViaTmpFromDisk RetrievalAllKeysSecure DefaultVerify newkey $ \tmp -> unVerified $ do
		oldobj <- calcRepo (gitAnnexLocation oldkey)
		isJust <$> linkOrCopy' (return True) newkey oldobj tmp Nothing
	, do
	 	{- The file being rekeyed is itself an unlocked file; if
		 - it's hard linked to the old key, that link must be broken. -}
		oldobj <- calcRepo (gitAnnexLocation oldkey)
		v <- tryNonAsync $ do
			st <- liftIO $ getFileStatus file
			when (linkCount st > 1) $ do
				freezeContent oldobj
				replaceFile file $ \tmp -> do
					unlessM (checkedCopyFile oldkey oldobj tmp Nothing) $
						error "can't lock old key"
					thawContent tmp
		ic <- withTSDelta (liftIO . genInodeCache file)
		case v of
			Left e -> do
				warning (show e)
				return False
			Right () -> do
				r <- linkToAnnex newkey file ic
				return $ case r of
					LinkAnnexFailed -> False
					LinkAnnexOk -> True
					LinkAnnexNoop -> True
	)

cleanup :: FilePath -> Key -> Key -> CommandCleanup
cleanup file oldkey newkey = do
	ifM (isJust <$> isAnnexLink file)
		( do
			-- Update symlink to use the new key.
			liftIO $ removeFile file
			addLink file newkey Nothing
		, do
			mode <- liftIO $ catchMaybeIO $ fileMode <$> getFileStatus file
			liftIO $ whenM (isJust <$> isPointerFile file) $
				writePointerFile file newkey mode
			stagePointerFile file mode =<< hashPointerFile newkey
			Database.Keys.removeAssociatedFile oldkey 
				=<< inRepo (toTopFilePath file)
		)
	whenM (inAnnex newkey) $
		logStatus newkey InfoPresent
	return True

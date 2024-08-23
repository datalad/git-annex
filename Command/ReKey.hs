{- git-annex command
 -
 - Copyright 2012-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.ReKey where

import Command
import qualified Annex
import Annex.Content
import Annex.Ingest
import Annex.Link
import Annex.Perms
import Annex.ReplaceFile
import Logs.Location
import Annex.InodeSentinal
import Annex.WorkTree
import Logs.Migrate
import Utility.InodeCache
import qualified Utility.RawFilePath as R

import System.PosixCompat.Files (linkCount, fileMode)

cmd :: Command
cmd = withAnnexOptions [jsonOptions] $ 
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
	<*> parseBatchOption False

-- Split on the last space, since a FilePath can contain whitespace,
-- but a Key very rarely does.
batchParser :: String -> Annex (Either String (RawFilePath, Key))
batchParser s = case separate (== ' ') (reverse s) of
	(rk, rf)
		| null rk || null rf -> return $ Left "Expected: \"file key\""
		| otherwise -> case deserializeKey (reverse rk) of
			Nothing -> return $ Left "bad key"
			Just k -> do
				let f = reverse rf
				f' <- liftIO $ relPathCwdToFile (toRawFilePath f)
				return $ Right (f', k)

seek :: ReKeyOptions -> CommandSeek
seek o = case batchOption o of
	Batch fmt -> batchOnly Nothing (reKeyThese o) $
		batchInput fmt batchParser
			(batchCommandAction . uncurry start)
	NoBatch -> withPairs 
		(\(si, p) -> commandAction (start si (parsekey p))) 
		(reKeyThese o)
  where
	parsekey (file, skey) =
		(toRawFilePath file, fromMaybe (giveup "bad key") (deserializeKey skey))

start :: SeekInput -> (RawFilePath, Key) -> CommandStart
start si (file, newkey) = lookupKey file >>= \case
	Just k -> go k
	Nothing -> stop
  where
	go oldkey
		| oldkey == newkey = stop
		| otherwise = starting "rekey" ai si $
			perform file oldkey newkey

	ai = ActionItemTreeFile file

perform :: RawFilePath -> Key -> Key -> CommandPerform
perform file oldkey newkey = do
	ifM (inAnnex oldkey) 
		( unlessM (linkKey file oldkey newkey) $
			giveup "failed creating link from old to new key"
		, unlessM (Annex.getRead Annex.force) $ do
			qp <- coreQuotePath <$> Annex.getGitConfig
			giveup $ decodeBS $ quote qp $ QuotedPath file
				<> " is not available (use --force to override)"
		)
	next $ cleanup file newkey $ const noop

{- Make a hard link to the old key content (when supported),
 - to avoid wasting disk space. -}
linkKey :: RawFilePath -> Key -> Key -> Annex Bool
linkKey file oldkey newkey = ifM (isJust <$> isAnnexLink file)
	( linkKey' DefaultVerify oldkey newkey
	, do
	 	{- The file being rekeyed is itself an unlocked file; if
		 - it's hard linked to the old key, that link must be broken. -}
		oldobj <- calcRepo (gitAnnexLocation oldkey)
		v <- tryNonAsync $ do
			st <- liftIO $ R.getFileStatus file
			when (linkCount st > 1) $ do
				freezeContent oldobj
				replaceWorkTreeFile (fromRawFilePath file) $ \tmp -> do
					unlessM (checkedCopyFile oldkey oldobj tmp Nothing) $
						giveup "can't lock old key"
					thawContent tmp
		ic <- withTSDelta (liftIO . genInodeCache file)
		case v of
			Left e -> do
				warning (UnquotedString (show e))
				return False
			Right () -> do
				r <- linkToAnnex newkey file ic
				return $ case r of
					LinkAnnexFailed -> False
					LinkAnnexOk -> True
					LinkAnnexNoop -> True
	)

 {- If the object file is already hardlinked to elsewhere, a hard
 - link won't be made by getViaTmpFromDisk, but a copy instead.
 - This avoids hard linking to content linked to an
 - unlocked file, which would leave the new key unlocked
 - and vulnerable to corruption. -}
linkKey' :: VerifyConfig -> Key -> Key -> Annex Bool
linkKey' v oldkey newkey =
	getViaTmpFromDisk RetrievalAllKeysSecure v newkey (AssociatedFile Nothing) $ \tmp -> unVerified $ do
		oldobj <- calcRepo (gitAnnexLocation oldkey)
		isJust <$> linkOrCopy' (return True) newkey oldobj tmp Nothing

cleanup :: RawFilePath -> Key -> (MigrationRecord -> Annex ()) -> CommandCleanup
cleanup file newkey a = do
	newkeyrec <- ifM (isJust <$> isAnnexLink file)
		( do
			-- Update symlink to use the new key.
			sha <- genSymlink file newkey Nothing
			stageSymlink file sha
			return (MigrationRecord sha)
		, do
			mode <- liftIO $ catchMaybeIO $ fileMode <$> R.getFileStatus file
			liftIO $ whenM (isJust <$> isPointerFile file) $
				writePointerFile file newkey mode
			sha <- hashPointerFile newkey
			stagePointerFile file mode sha
			return (MigrationRecord sha)
		)
	whenM (inAnnex newkey) $
		logStatus NoLiveUpdate newkey InfoPresent
	a newkeyrec
	return True

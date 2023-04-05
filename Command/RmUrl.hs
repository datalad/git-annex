{- git-annex command
 -
 - Copyright 2013-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.RmUrl where

import Command
import Logs.Web
import Annex.WorkTree

cmd :: Command
cmd = notBareRepo $
	command "rmurl" SectionCommon 
		"record file is not available at url"
		(paramRepeating (paramPair paramFile paramUrl))
		(seek <$$> optParser)

data RmUrlOptions = RmUrlOptions
	{ rmThese :: CmdParams
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser RmUrlOptions
optParser desc = RmUrlOptions
	<$> cmdParams desc
	<*> parseBatchOption False

seek :: RmUrlOptions -> CommandSeek
seek o = case batchOption o of
	Batch fmt -> batchOnly Nothing (rmThese o) $
		batchInput fmt batchParser (batchCommandAction . start)
	NoBatch -> withPairs (commandAction . start) (rmThese o)

-- Split on the last space, since a FilePath can contain whitespace,
-- but a url should not.
batchParser :: String -> Annex (Either String (FilePath, URLString))
batchParser s = case separate (== ' ') (reverse s) of
	(ru, rf)
		| null ru || null rf -> return $ Left "Expected: \"file url\""
		| otherwise -> do
			let f = reverse rf
			f' <- liftIO $ fromRawFilePath
				<$> relPathCwdToFile (toRawFilePath f)
			return $ Right (f', reverse ru)

start :: (SeekInput, (FilePath, URLString)) -> CommandStart
start (si, (file, url)) = lookupKeyStaged file' >>= \case
	Nothing -> stop
	Just key -> do
		let ai = mkActionItem (key, AssociatedFile (Just file'))
		starting "rmurl" ai si $
			next $ cleanup url key
  where
	file' = toRawFilePath file

cleanup :: String -> Key -> CommandCleanup
cleanup url key = do
	-- Remove the url, no matter what downloader.
	forM_ [minBound..maxBound] $ \dl -> 
		setUrlMissing key (setDownloader url dl)
	return True
	-- Unlike addurl, this does not update location tracking
	-- for remotes other than the web special remote. Doing so with
	-- a remote that git-annex can drop content from would rather
	-- unexpectedly leave content stranded on that remote.

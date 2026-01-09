{- git-annex command
 -
 - Copyright 2015-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.ReregisterUrl where

import Command
import Logs.Web
import Command.FromKey (keyOpt, keyOpt')
import qualified Remote

cmd :: Command
cmd = withAnnexOptions [jsonOptions] $ command "reregisterurl"
	SectionPlumbing "updates url registration information"
	(paramKey)
	(seek <$$> optParser)

data ReregisterUrlOptions = ReregisterUrlOptions
	{ keyOpts :: CmdParams
	, batchOption :: BatchMode
	, moveFromOption :: Maybe (DeferredParse Remote)
	}

optParser :: CmdParamsDesc -> Parser ReregisterUrlOptions
optParser desc = ReregisterUrlOptions
	<$> cmdParams desc
	<*> parseBatchOption False
	<*> optional (mkParseRemoteOption <$> parseMoveFromOption)

parseMoveFromOption :: Parser RemoteName
parseMoveFromOption = strOption
	( long "move-from" <> metavar paramRemote
	<> completeRemotes
	)

seek :: ReregisterUrlOptions -> CommandSeek
seek o = case (batchOption o, keyOpts o) of
	(Batch fmt, _) -> seekBatch o fmt
	(NoBatch, ps) -> commandAction (start o ps)

seekBatch :: ReregisterUrlOptions -> BatchFormat -> CommandSeek
seekBatch o fmt = batchOnly Nothing (keyOpts o) $
	batchInput fmt (pure . parsebatch) $
		batchCommandAction . start' o
  where
	parsebatch l = case keyOpt' l of
		Left e -> Left e
		Right k -> Right k

start :: ReregisterUrlOptions -> [String] -> CommandStart
start o (keyname:[]) = start' o (si, keyOpt keyname)
  where
	si = SeekInput [keyname]
start _ _ = giveup "specify a key"

start' :: ReregisterUrlOptions -> (SeekInput, Key) -> CommandStart
start' o (si, key) =
	starting "reregisterurl" ai si $
		perform o key
  where
	ai = ActionItemKey key

perform :: ReregisterUrlOptions -> Key -> CommandPerform
perform o key = maybe (pure Nothing) (Just <$$> getParsed) (moveFromOption o) >>= \case
	Nothing -> next $ return True
	Just r -> do
		us <- map fst
			. filter (\(_, d) -> d == OtherDownloader)
			. map getDownloader
			<$> getUrls key
		us' <- filterM (\u -> (== r) <$> Remote.claimingUrl u) us
		forM_ us' $ \u -> do
			setUrlMissing key (setDownloader u OtherDownloader)
			setUrlPresent key u
		next $ return True

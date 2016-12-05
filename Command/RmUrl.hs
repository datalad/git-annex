{- git-annex command
 -
 - Copyright 2013-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.RmUrl where

import Command
import Logs.Web
import qualified Remote
import CmdLine.Batch

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
	<*> parseBatchOption

seek :: RmUrlOptions -> CommandSeek
seek o = case batchOption o of
	Batch -> batchInput batchParser (batchCommandAction . start)
	NoBatch -> withPairs start (rmThese o)

-- Split on the last space, since a FilePath can contain whitespace,
-- but a url should not.
batchParser :: String -> Either String (FilePath, URLString)
batchParser s = case separate (== ' ') (reverse s) of
	(ru, rf)
		| null ru || null rf -> Left "Expected: \"file url\""
		| otherwise -> Right (reverse rf, reverse ru)

start :: (FilePath, URLString) -> CommandStart
start (file, url) = flip whenAnnexed file $ \_ key -> do
	showStart "rmurl" file
	next $ next $ cleanup url key

cleanup :: String -> Key -> CommandCleanup
cleanup url key = do
	r <- Remote.claimingUrl url
	setUrlMissing (Remote.uuid r) key (setDownloader' url r)
	return True

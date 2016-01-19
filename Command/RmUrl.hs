{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.RmUrl where

import Common.Annex
import Command
import Logs.Web
import Annex.UUID
import qualified Remote

cmd :: Command
cmd = notBareRepo $
	command "rmurl" SectionCommon 
		"record file is not available at url"
		(paramPair paramFile paramUrl)
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withPairs start

start :: (FilePath, String) -> CommandStart
start (file, url) = flip whenAnnexed file $ \_ key -> do
	showStart "rmurl" file
	next $ next $ cleanup url key

cleanup :: String -> Key -> CommandCleanup
cleanup url key = do
	r <- Remote.claimingUrl url
	setUrlMissing (Remote.uuid r) key (setDownloader' url r)
	return True

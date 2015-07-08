{- git-annex command
 -
 - Copyright 2010-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.ConfigList where

import Common.Annex
import Command
import Annex.UUID
import Annex.Init
import qualified Annex.Branch
import qualified Git.Config
import Remote.GCrypt (coreGCryptId)

cmd :: Command
cmd = noCommit $ 
	command "configlist" SectionPlumbing 
		"outputs relevant git configuration"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing start

start :: CommandStart
start = do
	u <- findOrGenUUID
	showConfig "annex.uuid" $ fromUUID u
	showConfig coreGCryptId =<< fromRepo (Git.Config.get coreGCryptId "")
	stop
  where
	showConfig k v = liftIO $ putStrLn $ k ++ "=" ++ v

{- The repository may not yet have a UUID; automatically initialize it
 - when there's a git-annex branch available. -}
findOrGenUUID :: Annex UUID
findOrGenUUID = do
	u <- getUUID
	if u /= NoUUID
		then return u
		else ifM Annex.Branch.hasSibling
			( do
				initialize Nothing
				getUUID
			, return NoUUID
			)

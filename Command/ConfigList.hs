{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.ConfigList where

import Common.Annex
import Command
import Annex.UUID
import qualified Git.Config
import Remote.GCrypt (coreGCryptId)

def :: [Command]
def = [noCommit $ command "configlist" paramNothing seek
	SectionPlumbing "outputs relevant git configuration"]

seek :: CommandSeek
seek = withNothing start

start :: CommandStart
start = do
	u <- getUUID
	showConfig "annex.uuid" $ fromUUID u
	showConfig coreGCryptId =<< fromRepo (Git.Config.get coreGCryptId "")
	stop
  where
  	showConfig k v = liftIO $ putStrLn $ k ++ "=" ++ v

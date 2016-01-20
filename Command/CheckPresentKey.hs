{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.CheckPresentKey where

import Command
import qualified Remote
import Annex
import Types.Messages

cmd :: Command
cmd = noCommit $ 
	command "checkpresentkey" SectionPlumbing
		"check if key is present in remote"
		(paramPair paramKey paramRemote)
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start (ks:rn:[]) = do
	setOutput QuietOutput
	maybe (error "Unknown remote") (go <=< flip Remote.hasKey k)
		=<< Remote.byNameWithUUID (Just rn)
  where
	k = fromMaybe (error "bad key") (file2key ks)
	go (Right True) = liftIO exitSuccess
	go (Right False) = liftIO exitFailure
	go (Left e) = liftIO $ do
		hPutStrLn stderr e
		exitWith $ ExitFailure 100
start _ = error "Wrong number of parameters"

{- git-annex-shell command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.LockContent where

import Command
import Annex.Content
import Remote.Helper.Ssh (contentLockedMarker)

cmd :: Command
cmd = noCommit $ 
	command "lockcontent" SectionPlumbing 
		"locks key's content in the annex, preventing it being dropped"
		paramKey
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

-- First, lock the content. Then, make sure the content is actually
-- present, and print out "OK". Wait for the caller to send a line before
-- dropping the lock.
start :: [String] -> CommandStart
start [ks] = do
	ok <- lockContentShared k (const locksuccess)
		`catchNonAsync` (const $ return False)
	liftIO $ if ok
		then exitSuccess
		else exitFailure
  where
	k = fromMaybe (error "bad key") (file2key ks)
	locksuccess = ifM (inAnnex k)
		( liftIO $ do
			putStrLn contentLockedMarker
			hFlush stdout
			_ <- getLine
			return True
		, return False
		)
start _ = error "Specify exactly 1 key."

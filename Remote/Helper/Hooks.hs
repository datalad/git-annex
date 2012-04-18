{- Adds hooks to remotes.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Hooks (addHooks) where

import qualified Data.Map as M

import Common.Annex
import Types.Remote
import qualified Annex
import Annex.LockPool
import Config

{- Modifies a remote's access functions to first run the
 - annex-start-command hook, and trigger annex-stop-command on shutdown.
 - This way, the hooks are only run when a remote is actively being used.
 -}
addHooks :: Remote -> Annex Remote
addHooks r = addHooks' r <$> lookupHook r "start" <*> lookupHook r "stop"
addHooks' :: Remote -> Maybe String -> Maybe String -> Remote
addHooks' r Nothing Nothing = r
addHooks' r starthook stophook = r'
	where
		r' = r
			{ storeKey = \k -> wrapper $ storeKey r k
			, retrieveKeyFile = \k f -> wrapper $ retrieveKeyFile r k f
			, retrieveKeyFileCheap = \k f -> wrapper $ retrieveKeyFileCheap r k f
			, removeKey = \k -> wrapper $ removeKey r k
			, hasKey = \k -> wrapper $ hasKey r k
			}
			where
				wrapper = runHooks r' starthook stophook

runHooks :: Remote -> Maybe String -> Maybe String -> Annex a -> Annex a
runHooks r starthook stophook a = do
	dir <- fromRepo gitAnnexRemotesDir
	let lck = dir </> remoteid ++ ".lck"
	whenM (not . any (== lck) . M.keys <$> getPool) $ do
		liftIO $ createDirectoryIfMissing True dir
		firstrun lck
	a
	where
		remoteid = show (uuid r)
		run Nothing = return ()
		run (Just command) = liftIO $ do
			_ <- boolSystem "sh" [Param "-c", Param command]
			return ()
		firstrun lck = do
			-- Take a shared lock; This indicates that git-annex
			-- is using the remote, and prevents other instances
			-- of it from running the stophook. If another
			-- instance is shutting down right now, this
			-- will block waiting for its exclusive lock to clear.
			lockFile lck

			-- The starthook is run even if some other git-annex
			-- is already running, and ran it before.
			-- It would be difficult to use locking to ensure
			-- it's only run once, and it's also possible for
			-- git-annex to be interrupted before it can run the
			-- stophook, in which case the starthook
			-- would be run again by the next git-annex.
			-- So, requiring idempotency is the right approach.
			run starthook

			Annex.addCleanup (remoteid ++ "-stop-command") $
				runstop lck
		runstop lck = do
			-- Drop any shared lock we have, and take an
			-- exclusive lock, without blocking. If the lock
			-- succeeds, we're the only process using this remote,
			-- so can stop it.
			unlockFile lck
			fd <- liftIO $ openFd lck ReadWrite (Just stdFileMode) defaultFileFlags
			v <- liftIO $ tryIO $
				setLock fd (WriteLock, AbsoluteSeek, 0, 0)
			case v of
				Left _ -> return ()
				Right _ -> run stophook
			liftIO $ closeFd fd

lookupHook :: Remote -> String -> Annex (Maybe String)
lookupHook r n = go =<< getRemoteConfig (repo r) hookname ""
	where
		go "" = return Nothing
		go command = return $ Just command
		hookname =  n ++ "-command"

{- Adds hooks to remotes.
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Remote.Helper.Hooks (addHooks) where

import qualified Data.Map as M

import Common.Annex
import Types.Remote
import Types.CleanupActions
import qualified Annex
import Annex.LockFile
import Annex.LockPool
#ifndef mingw32_HOST_OS
import Annex.Perms
#endif

{- Modifies a remote's access functions to first run the
 - annex-start-command hook, and trigger annex-stop-command on shutdown.
 - This way, the hooks are only run when a remote is actively being used.
 -}
addHooks :: Remote -> Remote
addHooks r = addHooks' r
	(remoteAnnexStartCommand $ gitconfig r)
	(remoteAnnexStopCommand $ gitconfig r)
addHooks' :: Remote -> Maybe String -> Maybe String -> Remote
addHooks' r Nothing Nothing = r
addHooks' r starthook stophook = r'
  where
	r' = r
		{ storeKey = \k f p -> wrapper $ storeKey r k f p
		, retrieveKeyFile = \k f d p -> wrapper $ retrieveKeyFile r k f d p
		, retrieveKeyFileCheap = \k af f -> wrapper $ retrieveKeyFileCheap r k af f
		, removeKey = wrapper . removeKey r
		, checkPresent = wrapper . checkPresent r
		}
	  where
		wrapper = runHooks r' starthook stophook

runHooks :: Remote -> Maybe String -> Maybe String -> Annex a -> Annex a
runHooks r starthook stophook a = do
	dir <- fromRepo gitAnnexRemotesDir
	let lck = dir </> remoteid ++ ".lck"
	whenM (notElem lck . M.keys <$> getLockCache) $ do
		liftIO $ createDirectoryIfMissing True dir
		firstrun lck
	a
  where
	remoteid = show (uuid r)
	run Nothing = noop
	run (Just command) = void $ liftIO $
		boolSystem "sh" [Param "-c", Param command]
	firstrun lck = do
		-- Take a shared lock; This indicates that git-annex
		-- is using the remote, and prevents other instances
		-- of it from running the stophook. If another
		-- instance is shutting down right now, this
		-- will block waiting for its exclusive lock to clear.
		lockFileCached lck

		-- The starthook is run even if some other git-annex
		-- is already running, and ran it before.
		-- It would be difficult to use locking to ensure
		-- it's only run once, and it's also possible for
		-- git-annex to be interrupted before it can run the
		-- stophook, in which case the starthook
		-- would be run again by the next git-annex.
		-- So, requiring idempotency is the right approach.
		run starthook

		Annex.addCleanup (StopHook $ uuid r) $ runstop lck
	runstop lck = do
		-- Drop any shared lock we have, and take an
		-- exclusive lock, without blocking. If the lock
		-- succeeds, we're the only process using this remote,
		-- so can stop it.
		unlockFile lck
#ifndef mingw32_HOST_OS
		mode <- annexFileMode
		v <- noUmask mode $ tryLockExclusive (Just mode) lck
#else
		v <- liftIO $ lockExclusive lck
#endif
		case v of
			Nothing -> noop
			Just lockhandle -> do
				run stophook
				liftIO $ dropLock lockhandle

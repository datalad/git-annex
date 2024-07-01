{- Adds hooks to remotes.
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Remote.Helper.Hooks (addHooks) where

import qualified Data.Map as M
import qualified System.FilePath.ByteString as P

import Annex.Common
import Types.Remote
import Types.CleanupActions
import qualified Annex
import Annex.LockFile
import Annex.LockPool
import Annex.Perms

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
		{ storeKey = \k af o p -> 
			wrapper $ storeKey r k af o p
		, retrieveKeyFile = \k f d p vc -> 
			wrapper $ retrieveKeyFile r k f d p vc
		, retrieveKeyFileCheap = case retrieveKeyFileCheap r of
			Just a -> Just $ \k af f -> wrapper $ a k af f
			Nothing -> Nothing
		, removeKey = wrapper . removeKey r
		, checkPresent = wrapper . checkPresent r
		}
	  where
		wrapper = runHooks r' starthook stophook

runHooks :: Remote -> Maybe String -> Maybe String -> Annex a -> Annex a
runHooks r starthook stophook a = do
	dir <- fromRepo gitAnnexRemotesDir
	let lck = dir P.</> remoteid <> ".lck"
	whenM (notElem lck . M.keys <$> getLockCache) $ do
		createAnnexDirectory dir
		firstrun lck
	a
  where
	remoteid = fromUUID (uuid r)
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

		Annex.addCleanupAction (StopHook $ uuid r) $ runstop lck
	runstop lck = do
		-- Drop any shared lock we have, and take an
		-- exclusive lock, without blocking. If the lock
		-- succeeds, we're the only process using this remote,
		-- so can stop it.
		unlockFile lck
#ifndef mingw32_HOST_OS
		mode <- annexFileMode
		v <- tryLockExclusive (Just mode) lck
#else
		v <- liftIO $ lockExclusive lck
#endif
		case v of
			Nothing -> noop
			Just lockhandle -> do
				run stophook
				liftIO $ dropLock lockhandle

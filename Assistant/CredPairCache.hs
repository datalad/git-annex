{- git-annex assistant CredPair cache.
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Assistant.CredPairCache (
	cacheCred,
	getCachedCred,
	expireCachedCred,
) where

import Assistant.Types.CredPairCache
import Types.Creds
import Assistant.Common
import Utility.ThreadScheduler

import qualified Data.Map as M
import Control.Concurrent

{- Caches a CredPair, but only for a limited time, after which it
 - will expire.
 -
 - Note that repeatedly caching the same CredPair
 - does not reset its expiry time.
 -}
cacheCred :: CredPair -> Seconds -> Assistant ()
cacheCred (login, password) expireafter = do
	cache <- getAssistant credPairCache
	liftIO $ do
		changeStrict cache $ M.insert login password
		void $ forkIO $ do
			threadDelaySeconds expireafter
			changeStrict cache $ M.delete login

getCachedCred :: Login -> Assistant (Maybe Password)
getCachedCred login = do
	cache <- getAssistant credPairCache
	liftIO $ M.lookup login <$> readMVar cache

expireCachedCred :: Login -> Assistant ()
expireCachedCred login = do
	cache <- getAssistant credPairCache
	liftIO $ changeStrict cache $ M.delete login

{- Update map strictly to avoid keeping references to old creds in memory. -}
changeStrict :: CredPairCache -> (M.Map Login Password -> M.Map Login Password) -> IO ()
changeStrict cache a = modifyMVar_ cache $ \m -> do
	let !m' = a m
	return m'

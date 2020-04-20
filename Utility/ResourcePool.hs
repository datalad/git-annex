{- Resource pools.
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE BangPatterns #-}

module Utility.ResourcePool (
	ResourcePool,
	mkResourcePool,
	mkResourcePoolNonConcurrent,
	withResourcePool,
	freeResourcePool,
) where

import Common

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Either

data ResourcePool r
	= ResourcePool Int (TVar Int) (TVar [r])
	| ResourcePoolNonConcurrent r

{- Make a new resource pool, that can grow to contain the specified number
 - of resources. -}
mkResourcePool :: MonadIO m => Int -> m (ResourcePool r)
mkResourcePool maxsz = liftIO $
	ResourcePool maxsz
		<$> newTVarIO 0
		<*> newTVarIO []

{- When there will not be multiple threads that may 
 - may concurrently try to use it, using this is more
 - efficient than mkResourcePool.
 -}
mkResourcePoolNonConcurrent :: (MonadMask m, MonadIO m) => m r -> m (ResourcePool r)
mkResourcePoolNonConcurrent allocresource =
	ResourcePoolNonConcurrent <$> allocresource

{- Runs an action with a resource.
 -
 - If no free resource is available in the pool,
 - will run the action the allocate a new resource if the pool's size
 - allows. Or will block a resource becomes available to use.
 -
 - The resource is returned to the pool at the end.
 -}
withResourcePool :: (MonadMask m, MonadIO m) => ResourcePool r -> m r -> (r -> m a) -> m a
withResourcePool (ResourcePoolNonConcurrent r) _ a = a r
withResourcePool (ResourcePool maxsz currsz p) allocresource a =
	bracket setup cleanup a
  where
	setup = do
		mr <- liftIO $ atomically $ do
			l <- readTVar p
			case l of
				(r:rs) -> do
					writeTVar p rs
					return (Just r)
				[] -> do
					n <- readTVar currsz
					if n < maxsz
						then do
							let !n' = succ n
							writeTVar currsz n'
							return Nothing
						else retry
		case mr of
			Just r -> return r
			Nothing -> allocresource
	cleanup r = liftIO $ atomically $ modifyTVar' p (r:)

{- Frees all resources in use in the pool, running the supplied action on
 - each. (If any of the actions throw an exception, it will be rethrown
 - after all the actions have completed.)
 -
 - The pool should not have any resources in use when this is called,
 - and the pool should not be used again after calling this.
 -}
freeResourcePool :: (MonadMask m, MonadIO m) => ResourcePool r -> (r -> m ()) -> m ()
freeResourcePool (ResourcePoolNonConcurrent r) freeresource = freeresource r
freeResourcePool (ResourcePool _ currsz p) freeresource = do
	rs <- liftIO $ atomically $ do
		writeTVar currsz 0
		swapTVar p []
	res <- forM rs $ tryNonAsync . freeresource
	case lefts res of
		[] -> return ()
		(e:_) -> throwM e


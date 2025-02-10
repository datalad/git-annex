{- git hash-object interface
 -
 - Copyright 2016-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.HashObject (
	hashFile,
	hashBlob,
	hashObjectStop,
	mkConcurrentHashObjectHandle,
	withHashObjectHandle,
) where

import Annex.Common
import qualified Git.HashObject
import qualified Annex
import Git.Types
import Utility.ResourcePool
import Types.Concurrency
import Annex.Concurrent.Utility

hashObjectStop :: Annex ()
hashObjectStop = maybe noop stop =<< Annex.getState Annex.hashobjecthandle
  where
	stop p = do
		liftIO $ freeResourcePool p Git.HashObject.hashObjectStop
		Annex.changeState $ \s -> s { Annex.hashobjecthandle = Nothing }

hashFile :: OsPath -> Annex Sha
hashFile f = withHashObjectHandle $ \h -> 
	liftIO $ Git.HashObject.hashFile h f

{- Note that the content will be written to a temp file.
 - So it may be faster to use Git.HashObject.hashObject for large
 - blob contents. -}
hashBlob :: Git.HashObject.HashableBlob b => b -> Annex Sha
hashBlob content = withHashObjectHandle $ \h ->
	liftIO $ Git.HashObject.hashBlob h content

withHashObjectHandle :: (Git.HashObject.HashObjectHandle -> Annex a) -> Annex a
withHashObjectHandle a =
	maybe mkpool go =<< Annex.getState Annex.hashobjecthandle
  where
	go p = withResourcePool p start a
	start = inRepo $ Git.HashObject.hashObjectStart True
	mkpool = do
		-- This only runs in non-concurrent code paths;
		-- a concurrent pool is set up earlier when needed.
		p <- mkResourcePoolNonConcurrent start
		Annex.changeState $ \s -> s { Annex.hashobjecthandle = Just p }
		go p

mkConcurrentHashObjectHandle :: Concurrency -> Annex (ResourcePool Git.HashObject.HashObjectHandle)
mkConcurrentHashObjectHandle c =
        Annex.getState Annex.hashobjecthandle >>= \case
                Just p@(ResourcePool {}) -> return p
                _ -> mkResourcePool =<< liftIO (maxHashObjects c)

{- git hash-object is typically CPU bound, and is not likely to be the main
 - bottleneck for any command. So limit to the number of CPU cores, maximum,
 - while respecting the -Jn value.
 -}
maxHashObjects :: Concurrency -> IO Int
maxHashObjects = concurrencyUpToCpus

{- git check-attr interface, with handle automatically stored in the Annex monad
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.CheckAttr (
	checkAttr,
	checkAttrStop,
	mkConcurrentCheckAttrHandle,
) where

import Annex.Common
import qualified Git.CheckAttr as Git
import qualified Annex
import Utility.ResourcePool
import Types.Concurrency
import GHC.Conc

{- All gitattributes used by git-annex. -}
annexAttrs :: [Git.Attr]
annexAttrs =
	[ "annex.backend"
	, "annex.numcopies"
	, "annex.largefiles"
	]

checkAttr :: Git.Attr -> FilePath -> Annex String
checkAttr attr file = withCheckAttrHandle $ \h -> 
	liftIO $ Git.checkAttr h attr file

withCheckAttrHandle :: (Git.CheckAttrHandle -> Annex a) -> Annex a
withCheckAttrHandle a = 
	maybe mkpool go =<< Annex.getState Annex.checkattrhandle
  where
	go p = withResourcePool p start a
	start = inRepo $ Git.checkAttrStart annexAttrs
	mkpool = do
		-- This only runs in non-concurrent code paths;
		-- a concurrent pool is set up earlier when needed.
		p <- mkResourcePoolNonConcurrent start
		Annex.changeState $ \s -> s { Annex.checkattrhandle = Just p }
		go p

mkConcurrentCheckAttrHandle :: Concurrency -> Annex (ResourcePool Git.CheckAttrHandle)
mkConcurrentCheckAttrHandle c =
	Annex.getState Annex.checkattrhandle >>= \case
		Just p@(ResourcePool {}) -> return p
		_ -> mkResourcePool =<< liftIO (maxCheckAttrs c)

{- git check-attr is typically CPU bound, and is not likely to be the main
 - bottleneck for any command. So limit to the number of CPU cores, maximum,
 - while respecting the -Jn value.
 -}
maxCheckAttrs :: Concurrency -> IO Int
maxCheckAttrs c = do
	let cn = case c of
		Concurrent n -> n
		NonConcurrent -> 1
		ConcurrentPerCpu -> 1
	pn <- liftIO getNumProcessors
	return (min cn pn)

checkAttrStop :: Annex ()
checkAttrStop = maybe noop stop =<< Annex.getState Annex.checkattrhandle
  where
	stop p = do
		liftIO $ freeResourcePool p Git.checkAttrStop
		Annex.changeState $ \s -> s { Annex.checkattrhandle = Nothing }

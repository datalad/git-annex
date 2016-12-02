{- P2P protocol, Annex implementation
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module P2P.Annex
	( RunEnv(..)
	, runFullProto
	) where

import Annex.Common
import Annex.Content
import P2P.Protocol
import P2P.IO

import Control.Monad.Free

-- Full interpreter for Proto, that can receive and send objects.
runFullProto :: RunEnv -> Proto a -> Annex (Maybe a)
runFullProto runenv = go
  where
	go :: RunProto Annex
	go (Pure v) = pure (Just v)
	go (Free (Net n)) = runNet runenv go n
	go (Free (Local l)) = runLocal runenv go l

runLocal :: RunEnv -> RunProto Annex -> LocalF (Proto a) -> Annex (Maybe a)
runLocal runenv runner f = case f of
	TmpContentSize k next -> do
		tmp <- fromRepo $ gitAnnexTmpObjectLocation k
		size <- liftIO $ catchDefaultIO 0 $ getFileSize tmp
		runner (next (Len size))

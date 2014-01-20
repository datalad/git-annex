{- git-annex numcopies log
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Logs.NumCopies where

import Common.Annex
import qualified Annex
import Logs
import Logs.SingleValue

instance Serializable Int where
	serialize = show
	deserialize = readish

setGlobalNumCopies :: Int -> Annex ()
setGlobalNumCopies = setLog numcopiesLog

{- Cached for speed. -}
getGlobalNumCopies :: Annex (Maybe Int)
getGlobalNumCopies = maybe numCopiesLoad (return . Just)
	=<< Annex.getState Annex.globalnumcopies

numCopiesLoad :: Annex (Maybe Int)
numCopiesLoad = do
	v <- getLog numcopiesLog
	Annex.changeState $ \s -> s { Annex.globalnumcopies = v }
	return v

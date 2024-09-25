{- git-annex numcopies log
 -
 - Copyright 2014-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Logs.NumCopies (
	setGlobalNumCopies,
	setGlobalMinCopies,
	getGlobalNumCopies,
	getGlobalMinCopies,
	globalNumCopiesLoad,
	globalMinCopiesLoad,
) where

import Annex.Common
import qualified Annex
import qualified Annex.Branch
import Types.NumCopies
import Logs
import Logs.SingleValue

instance SingleValueSerializable NumCopies where
	serialize = encodeBS . show . fromNumCopies
	deserialize = configuredNumCopies <$$> readish . decodeBS

instance SingleValueSerializable MinCopies where
	serialize = encodeBS . show . fromMinCopies
	deserialize = configuredMinCopies <$$> readish . decodeBS

setGlobalNumCopies :: NumCopies -> Annex ()
setGlobalNumCopies new = do
	curr <- getGlobalNumCopies
	when (curr /= Just new) $
		setLog (Annex.Branch.RegardingUUID []) numcopiesLog new
	Annex.changeState $ \s -> s { Annex.globalnumcopies = Nothing }

setGlobalMinCopies :: MinCopies -> Annex ()
setGlobalMinCopies new = do
	curr <- getGlobalMinCopies
	when (curr /= Just new) $
		setLog (Annex.Branch.RegardingUUID []) mincopiesLog new
	Annex.changeState $ \s -> s { Annex.globalmincopies = Nothing }

{- Value configured in the numcopies log. Cached for speed. -}
getGlobalNumCopies :: Annex (Maybe NumCopies)
getGlobalNumCopies = maybe globalNumCopiesLoad return
	=<< Annex.getState Annex.globalnumcopies

{- Value configured in the mincopies log. Cached for speed. -}
getGlobalMinCopies :: Annex (Maybe MinCopies)
getGlobalMinCopies = maybe globalMinCopiesLoad return
	=<< Annex.getState Annex.globalmincopies

globalNumCopiesLoad :: Annex (Maybe NumCopies)
globalNumCopiesLoad = do
	v <- getLog numcopiesLog
	Annex.changeState $ \s -> s { Annex.globalnumcopies = Just v }
	return v

globalMinCopiesLoad :: Annex (Maybe MinCopies)
globalMinCopiesLoad = do
	v <- getLog mincopiesLog
	Annex.changeState $ \s -> s { Annex.globalmincopies = Just v }
	return v

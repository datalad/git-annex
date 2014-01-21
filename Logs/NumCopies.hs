{- git-annex numcopies log
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Logs.NumCopies (
	module Types.NumCopies,
	setGlobalNumCopies,
	getGlobalNumCopies,
	globalNumCopiesLoad,
	getFileNumCopies,
	numCopiesCheck,
	getNumCopies,
	deprecatedNumCopies,
) where

import Common.Annex
import qualified Annex
import Types.NumCopies
import Logs
import Logs.SingleValue
import Logs.Trust
import Annex.CheckAttr
import qualified Remote

instance SingleValueSerializable NumCopies where
	serialize (NumCopies n) = show n
	deserialize = NumCopies <$$> readish

setGlobalNumCopies :: NumCopies -> Annex ()
setGlobalNumCopies = setLog numcopiesLog

{- Cached for speed. -}
getGlobalNumCopies :: Annex (Maybe NumCopies)
getGlobalNumCopies = maybe globalNumCopiesLoad (return . Just)
	=<< Annex.getState Annex.globalnumcopies

globalNumCopiesLoad :: Annex (Maybe NumCopies)
globalNumCopiesLoad = do
	v <- getLog numcopiesLog
	Annex.changeState $ \s -> s { Annex.globalnumcopies = v }
	return v

{- Numcopies value for a file, from .gitattributes or global,
 - but not the deprecated git config. -}
getFileNumCopies :: FilePath  -> Annex (Maybe NumCopies)
getFileNumCopies file = do
	global <- getGlobalNumCopies
	case global of
		Just n -> return $ Just n
		Nothing -> (NumCopies <$$> readish)
			<$> checkAttr "annex.numcopies" file

deprecatedNumCopies :: Annex NumCopies
deprecatedNumCopies = NumCopies . fromMaybe 1 . annexNumCopies
	<$> Annex.getGitConfig

{- Checks if numcopies are satisfied by running a comparison
 - between the number of (not untrusted) copies that are
 - belived to exist, and the configured value.
 -
 - Includes the deprecated annex.numcopies git config if
 - nothing else specifies a numcopies value. -}
numCopiesCheck :: FilePath -> Key -> (Int -> Int -> v) -> Annex v
numCopiesCheck file key vs = do
	numcopiesattr <- getFileNumCopies file
	NumCopies needed <- getNumCopies numcopiesattr
	have <- trustExclude UnTrusted =<< Remote.keyLocations key
	return $ length have `vs` needed

getNumCopies :: Maybe NumCopies -> Annex NumCopies
getNumCopies (Just v) = return v
getNumCopies Nothing = deprecatedNumCopies

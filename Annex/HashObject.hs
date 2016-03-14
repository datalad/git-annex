{- git hash-object interface, with handle automatically stored in the Annex monad
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.HashObject (
	hashFile,
	hashBlob,
	hashObjectHandle,
	hashObjectStop,
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import System.PosixCompat.Types

import Annex.Common
import qualified Git
import qualified Git.HashObject
import qualified Annex
import Git.Types
import Git.FilePath
import qualified Git.Ref
import Annex.Link

hashObjectHandle :: Annex Git.HashObject.HashObjectHandle
hashObjectHandle = maybe startup return =<< Annex.getState Annex.hashobjecthandle
  where
	startup = do
		inRepo $ Git.hashObjectStart
		Annex.changeState $ \s -> s { Annex.hashobjecthandle = Just h }
		return h

hashObjectStop :: Annex ()
hashObjectStop = maybe noop stop =<< Annex.hashobjecthandle
  where
	stop h = do
		liftIO $ Git.hashObjectStop h
		Annex.changeState $ \s -> s { Annex.hashobjecthandle = Nothing }

hashFile :: FilePath -> Annex Sha
hashFile f = do
	h <- hashObjectHandle
	Git.HashObject.hashFile h f

{- Note that the content will be written to a temp file.
 - So it may be faster to use Git.HashObject.hashObject for large
 - blob contents. -}
hashBlob :: String -> Annex Sha
hashBlob content = do
	h <- hashObjectHandle
	Git.HashObject.hashFile h content

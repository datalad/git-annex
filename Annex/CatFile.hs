{- git cat-file interface, with handle automatically stored in the Annex monad
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.CatFile (
	catFile
) where

import Common.Annex
import qualified Git.CatFile
import qualified Annex

catFile :: String -> FilePath -> Annex String
catFile branch file = maybe startup go =<< Annex.getState Annex.catfilehandle
	where
		startup = do
			h <- inRepo $ Git.CatFile.catFileStart
			Annex.changeState $ \s -> s { Annex.catfilehandle = Just h }
			go h
		go h = liftIO $ Git.CatFile.catFile h branch file

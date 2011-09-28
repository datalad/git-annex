{- git cat-file interface, with handle automatically stored in the Annex monad
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CatFile (
	catFile
) where

import Control.Monad.State

import qualified Git.CatFile
import Types
import qualified Annex

catFile :: String -> FilePath -> Annex String
catFile branch file = maybe startup go =<< Annex.getState Annex.catfilehandle
	where
		startup = do
			g <- Annex.gitRepo
			h <- liftIO $ Git.CatFile.catFileStart g
			Annex.changeState $ \s -> s { Annex.catfilehandle = Just h }
			go h
		go h = liftIO $ Git.CatFile.catFile h branch file

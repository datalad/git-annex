{- git check-ignore interface, with handle automatically stored in
 - the Annex monad
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.CheckIgnore (
	checkIgnored,
	checkIgnoreHandle,
	checkIgnoreStop
) where

import Annex.Common
import qualified Git.CheckIgnore as Git
import qualified Annex

checkIgnored :: FilePath -> Annex Bool
checkIgnored file = go =<< checkIgnoreHandle
  where
	go h = liftIO $ Git.checkIgnored h file

checkIgnoreHandle :: Annex Git.CheckIgnoreHandle
checkIgnoreHandle = maybe startup return =<< Annex.getState Annex.checkignorehandle
  where
	startup = do
		h <- inRepo Git.checkIgnoreStart
		Annex.changeState $ \s -> s { Annex.checkignorehandle = Just h }
		return h

checkIgnoreStop :: Annex ()
checkIgnoreStop = maybe noop stop =<< Annex.getState Annex.checkignorehandle
  where
	stop h = do
		liftIO $ Git.checkIgnoreStop h
		Annex.changeState $ \s -> s { Annex.checkignorehandle = Nothing }

{- git check-ignore interface, with handle automatically stored in
 - the Annex monad
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
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
	go Nothing = return False
	go (Just h) = liftIO $ Git.checkIgnored h file

checkIgnoreHandle :: Annex (Maybe Git.CheckIgnoreHandle)
checkIgnoreHandle = maybe startup return =<< Annex.getState Annex.checkignorehandle
  where
	startup = do
		v <- inRepo Git.checkIgnoreStart
		when (isNothing v) $
			warning "The installed version of git is too old for .gitignores to be honored by git-annex."
		Annex.changeState $ \s -> s { Annex.checkignorehandle = Just v }
		return v

checkIgnoreStop :: Annex ()
checkIgnoreStop = maybe noop stop =<< Annex.getState Annex.checkignorehandle
  where
	stop (Just h) = do
		liftIO $ Git.checkIgnoreStop h
		Annex.changeState $ \s -> s { Annex.checkignorehandle = Nothing }
	stop Nothing = noop

{- git check-attr interface, with handle automatically stored in the Annex monad
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.CheckAttr (
	checkAttr,
	checkAttrHandle,
	checkAttrStop,
) where

import Annex.Common
import qualified Git.CheckAttr as Git
import qualified Annex

{- All gitattributes used by git-annex. -}
annexAttrs :: [Git.Attr]
annexAttrs =
	[ "annex.backend"
	, "annex.numcopies"
	, "annex.largefiles"
	]

checkAttr :: Git.Attr -> FilePath -> Annex String
checkAttr attr file = do
	h <- checkAttrHandle
	liftIO $ Git.checkAttr h attr file

checkAttrHandle :: Annex Git.CheckAttrHandle
checkAttrHandle = maybe startup return =<< Annex.getState Annex.checkattrhandle
  where
	startup = do
		h <- inRepo $ Git.checkAttrStart annexAttrs
		Annex.changeState $ \s -> s { Annex.checkattrhandle = Just h }
		return h

checkAttrStop :: Annex ()
checkAttrStop = maybe noop stop =<< Annex.getState Annex.checkattrhandle
  where
	stop h = do
		liftIO $ Git.checkAttrStop h
		Annex.changeState $ \s -> s { Annex.checkattrhandle = Nothing }

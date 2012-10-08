{- git-annex control over whether content is wanted
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Wanted where

import Common.Annex
import qualified Remote
import Annex.Content
import Logs.PreferredContent
import Git.FilePath
import qualified Annex
import Annex.UUID

import qualified Data.Set as S

checkAuto :: (Bool -> Annex Bool) -> Annex Bool
checkAuto a = Annex.getState Annex.auto >>= a

{- A file's content should be gotten if it's not already present.
 - In auto mode, only get files that are preferred content. -}
shouldGet :: FilePath -> Key -> Bool -> Annex Bool
shouldGet file key auto = (not <$> inAnnex key) <&&> want
	where
		want
			| auto = do
				fp <- inRepo $ toTopFilePath file
				isPreferredContent Nothing S.empty fp
			| otherwise = return True

{- A file's content should be sent to a remote.
 - In auto mode, only send files that are preferred content of the remote. -}
shouldSend :: Remote -> FilePath -> Bool -> Annex Bool
shouldSend _ _ False = return True
shouldSend to file True = do
	fp <- inRepo $ toTopFilePath file
	isPreferredContent (Just $ Remote.uuid to) S.empty fp

{- A file's content should be dropped normally.
 - (This does not check numcopies though.)
 - In auto mode, hold on to preferred content. -}
shouldDrop :: Maybe Remote -> FilePath -> Bool -> Annex Bool
shouldDrop _ _ False = return True
shouldDrop from file True = do
	fp <- inRepo $ toTopFilePath file
	u <- maybe getUUID (return . Remote.uuid) from
	not <$> isPreferredContent (Just u) (S.singleton u) fp

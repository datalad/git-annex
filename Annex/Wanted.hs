{- git-annex checking whether content is wanted
 -
 - Copyright 2012-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Wanted where

import Annex.Common
import Logs.PreferredContent
import Annex.UUID
import Annex.CatFile
import Git.FilePath
import qualified Database.Keys

import qualified Data.Set as S

{- Check if a file is preferred content for the local repository. -}
wantGet :: Bool -> Maybe Key -> AssociatedFile -> Annex Bool
wantGet d key file = isPreferredContent Nothing S.empty key file d

{- Check if a file is preferred content for a remote. -}
wantSend :: Bool -> Maybe Key -> AssociatedFile -> UUID -> Annex Bool
wantSend d key file to = isPreferredContent (Just to) S.empty key file d

{- Check if a file can be dropped, maybe from a remote.
 - Don't drop files that are preferred content.
 -
 - The AssociatedFile is the one that the user requested to drop.
 - There may be other files that use the same key, and preferred content
 - may match some of those and not others. If any are preferred content,
 - that will prevent dropping. When the other associated files are known,
 - they can be provided, otherwise this looks them up.
 -}
wantDrop :: Bool -> Maybe UUID -> Maybe Key -> AssociatedFile -> (Maybe [AssociatedFile]) -> Annex Bool
wantDrop d from key file others = do
	u <- maybe getUUID (pure . id) from
	let s = S.singleton u
	let checkwant f = isPreferredContent (Just u) s key f d
	ifM (checkwant file)
		( return False
		, do
			others' <- case others of
				Just afs -> pure (filter (/= file) afs)
				Nothing -> case key of
					Just k -> mapM (\f -> AssociatedFile . Just <$> fromRepo (fromTopFilePath f))
						=<< Database.Keys.getAssociatedFiles k
					Nothing -> pure []
			l <- filterM checkwant others'
			if null l
				then return True
				else checkassociated l
		)
  where
	-- Some associated files that are in the keys database may no
	-- longer correspond to files in the repository, and should
	-- not prevent dropping.
	checkassociated [] = return True
	checkassociated (AssociatedFile (Just af):fs) =
		catKeyFile af >>= \case
			Just k | Just k == key -> return False
			_ -> checkassociated fs
	checkassociated (AssociatedFile Nothing:fs) = checkassociated fs

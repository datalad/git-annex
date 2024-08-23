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
import Types.FileMatcher

import qualified Data.Set as S

{- Check if a file is preferred content for the local repository. -}
wantGet :: LiveUpdate -> Bool -> Maybe Key -> AssociatedFile -> Annex Bool
wantGet lu d key file = isPreferredContent lu Nothing S.empty key file d

{- Check if a file is preferred content for a repository. -}
wantGetBy :: LiveUpdate -> Bool -> Maybe Key -> AssociatedFile -> UUID -> Annex Bool
wantGetBy lu d key file to = isPreferredContent lu (Just to) S.empty key file d

{- Check if a file is not preferred or required content, and can be
 - dropped. When a UUID is provided, checks for that repository.
 -
 - The AssociatedFile is the one that the user requested to drop.
 - There may be other files that use the same key, and preferred content
 - may match some of those and not others. If any are preferred content,
 - that will prevent dropping. When the other associated files are known,
 - they can be provided, otherwise this looks them up.
 -}
wantDrop :: LiveUpdate -> Bool -> Maybe UUID -> Maybe Key -> AssociatedFile -> (Maybe [AssociatedFile]) -> Annex Bool
wantDrop lu d from key file others =
	isNothing <$> checkDrop isPreferredContent lu d from key file others

{- Generalization of wantDrop that can also be used with isRequiredContent.
 -
 - When the content should not be dropped, returns Just the file that
 - the checker matches.
 -}
checkDrop :: (LiveUpdate -> Maybe UUID -> AssumeNotPresent -> Maybe Key -> AssociatedFile -> Bool -> Annex Bool) -> LiveUpdate -> Bool -> Maybe UUID -> Maybe Key -> AssociatedFile -> (Maybe [AssociatedFile]) -> Annex (Maybe AssociatedFile)
checkDrop checker lu d from key file others = do
	u <- maybe getUUID (pure . id) from
	let s = S.singleton u
	let checker' f = checker lu (Just u) s key f d
	ifM (checker' file)
		( return (Just file)
		, do
			others' <- case others of
				Just afs -> pure (filter (/= file) afs)
				Nothing -> case key of
					Just k ->
						mapM (\f -> AssociatedFile . Just <$> fromRepo (fromTopFilePath f))
							=<< Database.Keys.getAssociatedFiles k
					Nothing -> pure []
			l <- filterM checker' others'
			if null l
				then return Nothing
				else checkassociated l
		)
  where
	-- Some associated files that are in the keys database may no
	-- longer correspond to files in the repository, and should
	-- not prevent dropping.
	checkassociated [] = return Nothing
	checkassociated (af@(AssociatedFile (Just f)):fs) =
		catKeyFile f >>= \case
			Just k | Just k == key -> return (Just af)
			_ -> checkassociated fs
	checkassociated (AssociatedFile Nothing:fs) = checkassociated fs

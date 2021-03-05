{- git-annex exports
 -
 - Copyright 2017-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.Export where

import Annex
import Annex.CatFile
import Types
import Types.Key
import qualified Git
import qualified Types.Remote as Remote
import Messages

import Data.Maybe

-- From a sha pointing to the content of a file to the key
-- to use to export it. When the file is annexed, it's the annexed key.
-- When the file is stored in git, it's a special type of key to indicate
-- that.
exportKey :: Git.Sha -> Annex Key
exportKey sha = mk <$> catKey sha
  where
	mk (Just k) = k
	mk Nothing = gitShaKey sha

-- Encodes a git sha as a key. This is used to represent a non-annexed
-- file that is stored on a special remote, which necessarily needs a
-- key.
--
-- This is not the same as a SHA1 key, because the mapping needs to be
-- bijective, also because git may not always use SHA1, and because git
-- takes a SHA1 of the file size + content, while git-annex SHA1 keys
-- only checksum the content.
gitShaKey :: Git.Sha -> Key
gitShaKey (Git.Ref s) = mkKey $ \kd -> kd
	{ keyName = s
	, keyVariety = OtherKey "GIT"
	}

-- Reverse of gitShaKey
keyGitSha :: Key -> Maybe Git.Sha
keyGitSha k
	| fromKey keyVariety k == OtherKey "GIT" =
		Just (Git.Ref (fromKey keyName k))
	| otherwise = Nothing

-- Is a key storing a git sha, and not used for an annexed file?
isGitShaKey :: Key -> Bool
isGitShaKey = isJust . keyGitSha

warnExportImportConflict :: Remote -> Annex ()
warnExportImportConflict r = do
	isimport <- Remote.isImportSupported r
	isexport <- Remote.isExportSupported r
	let (ops, resolvcmd) = case (isexport, isimport) of
		(False, True) -> ("imported from", "git-annex import")
		(True, False) -> ("exported to", "git-annex export")
		_ -> ("exported to and/or imported from", "git-annex export")
	toplevelWarning True $ unwords
		[ "Conflict detected. Different trees have been"
		, ops, Remote.name r ++ ". Use"
		, resolvcmd
		, "to resolve this conflict."
		]

{- Encoding a git sha as a Key
 -
 - Copyright 2017-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.GitShaKey where

import Types
import Types.Key
import qualified Git

import Data.Maybe
import qualified Data.ByteString.Short as S (fromShort, toShort)

-- Encodes a git sha as a Key. This is used to represent a non-annexed
-- file. For example, when storing a git sha on a special remote.
--
-- This is not the same as a SHA1 key, because the mapping needs to be
-- bijective, also because git may not always use SHA1, and because git
-- takes a SHA1 of the file size + content, while git-annex SHA1 keys
-- only checksum the content.
gitShaKey :: Git.Sha -> Key
gitShaKey (Git.Ref s) = mkKey $ \kd -> kd
	{ keyName = S.toShort s
	, keyVariety = OtherKey "GIT"
	}

-- Reverse of gitShaKey
keyGitSha :: Key -> Maybe Git.Sha
keyGitSha k
	| fromKey keyVariety k == OtherKey "GIT" =
		Just (Git.Ref (S.fromShort (fromKey keyName k)))
	| otherwise = Nothing

-- Is a key storing a git sha, and not used for an annexed file?
isGitShaKey :: Key -> Bool
isGitShaKey = isJust . keyGitSha

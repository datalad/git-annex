{- git-remote-annex types
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.GitRemoteAnnex 
	( Manifest
	, mkManifest
	, inManifest
	, outManifest
	) where

import Types.Key

import qualified Data.Set as S

-- The manifest contains an ordered list of git bundle keys.
--
-- There is a second list of git bundle keys that are no longer
-- used and should be deleted. This list should never contain keys
-- that are in the first list.
data Manifest =
	Manifest 
		{ inManifest :: [Key]
		, outManifest :: S.Set Key
		}
	deriving (Show)

-- Smart constructor for Manifest. Preserves outManifest invariant.
mkManifest
	:: [Key] -- ^ inManifest
	-> S.Set Key -- ^ outManifest
	-> Manifest
mkManifest inks outks = Manifest inks (S.filter (`notElem` inks) outks)

instance Monoid Manifest where
	mempty = Manifest mempty mempty

instance Semigroup Manifest where
	a <> b = mkManifest
		(inManifest a <> inManifest b)
		(S.union (outManifest a) (outManifest b))

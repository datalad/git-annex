{- git-annex remote list utils
 -
 - Copyright 2011-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.List.Util where

import Annex.Common
import qualified Annex
import qualified Git.Config
import Annex.UUID
import Types.Remote
import Config.DynamicConfig

import Data.Ord

{- Call when remotes have changed. Re-reads the git config, and
 - invalidates the cache so the remoteList will be re-generated next time
 - it's used. -}
remotesChanged :: Annex ()
remotesChanged = do
	newg <- inRepo Git.Config.reRead
	Annex.changeState $ \s -> s 
		{ Annex.remotes = []
		, Annex.gitremotes = Nothing
		, Annex.repo = newg
		}

{- Whether to include remotes that have annex-ignore set. -}
newtype IncludeIgnored = IncludeIgnored Bool

keyPossibilities'
	:: IncludeIgnored
	-> Key
	-> [UUID]
	-- ^ uuids of remotes that are recorded to have the key
	-> [Remote]
	-- ^ all remotes
	-> Annex [Remote]
keyPossibilities' ii _key remotelocations rs = do
	u <- getUUID
	let locations = filter (/= u) remotelocations
	let speclocations = map uuid
		$ filter (remoteAnnexSpeculatePresent . gitconfig) rs
	-- there are unlikely to be many speclocations, so building a Set
	-- is not worth the expense
	let locations' = speclocations ++ filter (`notElem` speclocations) locations
	fst <$> remoteLocations' ii locations' [] rs

remoteLocations' :: IncludeIgnored -> [UUID] -> [UUID] -> [Remote] -> Annex ([Remote], [UUID])
remoteLocations' (IncludeIgnored ii) locations trusted rs = do
	let validtrustedlocations = nub locations `intersect` trusted

	-- remotes that match uuids that have the key
	allremotes <- if not ii
			then filterM (not <$$> liftIO . getDynamicConfig . remoteAnnexIgnore . gitconfig) rs
			else return rs
	let validremotes = remotesWithUUID allremotes locations

	return (sortBy (comparing cost) validremotes, validtrustedlocations)

{- Filters a list of remotes to ones that have the listed uuids. -}
remotesWithUUID :: [Remote] -> [UUID] -> [Remote]
remotesWithUUID rs us = filter (\r -> uuid r `elem` us) rs

{- Filters a list of remotes to ones that do not have the listed uuids. -}
remotesWithoutUUID :: [Remote] -> [UUID] -> [Remote]
remotesWithoutUUID rs us = filter (\r -> uuid r `notElem` us) rs

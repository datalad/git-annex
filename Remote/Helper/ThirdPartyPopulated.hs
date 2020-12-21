{- Helpers for thirdPartyPopulated remotes
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.Helper.ThirdParty where

import Annex.Common
import Types.Remote
import Types.Import
import Crypto (isEncKey)
import Utility.Metered

import qualified System.FilePath.ByteString as P

-- When a remote is thirdPartyPopulated, and contains a backup of a
-- git-annex repository or some special remotes, this can be used to
-- find only those ImportLocations that are annex object files.
-- All other ImportLocations are ignored.
importKey :: ImportLocation -> ContentIdentifier -> ByteSize -> MeterUpdate -> Annex (Maybe Key)
importKey loc _cid sz _ = return $ case deserializeKey' f of
	Just k
		-- Annex objects always are in a subdirectory with the same
		-- name as the filename. If this is not the case for the file
		-- that was backed up, it is probably not a valid annex object.
		-- Eg, it could be something in annex/bad/, or annex/tmp/.
		-- Or it could be a file that only happens to have a name
		-- like an annex object.
		-- (This does unfortunately prevent recognizing files that are
		-- part of special remotes that don't use that layout. The most
		-- likely special remote to be in a backup, the directory
		-- special remote, does use that layout at least.)
		| lastMaybe (P.splitDirectories (P.dropFileName p)) /= Just f -> Nothing
		-- Chunked or encrypted keys used in special remotes are not
		-- supported.
		| isChunkKey k || isEncKey k -> Nothing
		-- Check that the size of the key is the same as the size of the
		-- file stored in the backup. This is a cheap way to make sure it's
		-- probabably the actual content of the file. We don't fully
		-- verify the content here because that could be a very 
		-- expensive operation for a large repository; if the user
		-- wants to detect every possible data corruption problem
		-- (eg, wrong data read off disk during backup, or the object
		-- was corrupt in the git-annex repo and that bad object got
		-- backed up), they can fsck the remote.
		| otherwise -> case fromKey keySize k of
			Just sz'
				| sz' == sz -> Just k
				| otherwise -> Nothing
			Nothing -> Just k
	Nothing -> Nothing
  where
	p = fromImportLocation loc
	f = P.takeFileName p

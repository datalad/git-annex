{- Helpers for thirdPartyPopulated remotes
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Remote.Helper.ThirdPartyPopulated where

import Annex.Common
import Types.Remote
import Types.Import
import Crypto (isEncKey)
import Utility.Metered

import qualified System.FilePath.ByteString as P
import qualified Data.ByteString as S

-- When a remote is thirdPartyPopulated, the files we want are probably
-- in the .git directory. But, git does not really support .git in paths
-- in a git tree. (Such a tree can be built, but it will lead to problems.)
-- And so anything in .git is prevented from being imported.
-- To work around that, this renames that directory when generating an
-- ImportLocation.
mkThirdPartyImportLocation :: RawFilePath -> ImportLocation
mkThirdPartyImportLocation =
	mkImportLocation . P.joinPath . map esc . P.splitDirectories
  where
	esc ".git" = "dotgit"
	esc x
		| "dotgit" `S.isSuffixOf` x = "dot" <> x
		| otherwise = x

fromThirdPartyImportLocation :: ImportLocation -> RawFilePath
fromThirdPartyImportLocation =
	P.joinPath . map unesc . P.splitDirectories . fromImportLocation
  where
	unesc "dotgit" = ".git"
	unesc x
		| "dotgit" `S.isSuffixOf` x = S.drop 3 x
		| otherwise = x

-- When a remote is thirdPartyPopulated, and contains a backup of a
-- git-annex repository or some special remotes, this can be used to
-- find only those ImportLocations that are annex object files.
-- All other ImportLocations are ignored.
importKey :: ImportLocation -> ContentIdentifier -> ByteSize -> MeterUpdate -> Annex (Maybe Key)
importKey loc _cid sz _ = return $ importKey' loc (Just sz)

importKey' :: ImportLocation -> Maybe ByteSize -> Maybe Key
importKey' loc msz = case fileKey f of
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
		| otherwise -> case (msz, fromKey keySize k) of
			(Just sz, Just sz')
				| sz' == sz -> Just k
				| otherwise -> Nothing
			_ -> Just k
	Nothing -> Nothing
  where
	p = fromImportLocation loc
	f = P.takeFileName p

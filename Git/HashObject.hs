{- git hash-object interface
 -
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Git.HashObject where

import Common
import Git
import Git.Sha
import Git.Command
import Git.Types
import qualified Utility.CoProcess as CoProcess
import Utility.Tmp

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder

type HashObjectHandle = CoProcess.CoProcessHandle

hashObjectStart :: Bool -> Repo -> IO HashObjectHandle
hashObjectStart writeobject = gitCoProcessStart True $ catMaybes
	[ Just (Param "hash-object")
	, if writeobject then Just (Param "-w") else Nothing
	, Just (Param "--stdin-paths")
	, Just (Param "--no-filters")
	]

hashObjectStop :: HashObjectHandle -> IO ()
hashObjectStop = CoProcess.stop

{- Injects a file into git, returning the Sha of the object. -}
hashFile :: HashObjectHandle -> FilePath -> IO Sha
hashFile h file = CoProcess.query h send receive
  where
	send to = hPutStrLn to =<< absPath file
	receive from = getSha "hash-object" $ hGetLine from

class HashableBlob t where
	hashableBlobToHandle :: Handle -> t -> IO ()

instance HashableBlob L.ByteString where
	hashableBlobToHandle = L.hPut

instance HashableBlob S.ByteString where
	hashableBlobToHandle = S.hPut

instance HashableBlob Builder where
	hashableBlobToHandle = hPutBuilder

{- Injects a blob into git. Unfortunately, the current git-hash-object
 - interface does not allow batch hashing without using temp files. -}
hashBlob :: HashableBlob b => HashObjectHandle -> b -> IO Sha
hashBlob h b = withTmpFile "hash" $ \tmp tmph -> do
	hashableBlobToHandle tmph b
	hClose tmph
	hashFile h tmp

{- Injects some content into git, returning its Sha.
 - 
 - Avoids using a tmp file, but runs a new hash-object command each
 - time called. -}
hashObject :: ObjectType -> String -> Repo -> IO Sha
hashObject objtype content = hashObject' objtype (flip hPutStr content)

hashObject' :: ObjectType -> (Handle -> IO ()) -> Repo -> IO Sha
hashObject' objtype writer repo = getSha subcmd $
	pipeWriteRead (map Param params) (Just writer) repo
  where
	subcmd = "hash-object"
	params = [subcmd, "-t", decodeBS (fmtObjectType objtype), "-w", "--stdin", "--no-filters"]

{- git hash-object interface
 -
 - Copyright 2011-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.HashObject where

import Common
import Git
import Git.Sha
import Git.Command
import Git.Types
import Utility.Tmp
import qualified Utility.CoProcess as CoProcess
import qualified Utility.OsString as OS

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder

data HashObjectHandle = HashObjectHandle CoProcess.CoProcessHandle Repo [CommandParam]

hashObjectStart :: Bool -> Repo -> IO HashObjectHandle
hashObjectStart writeobject repo = do
	h <- gitCoProcessStart True (ps ++ [Param "--stdin-paths"]) repo
	return (HashObjectHandle h repo ps)
  where
	ps = catMaybes
		[ Just (Param "hash-object")
		, if writeobject then Just (Param "-w") else Nothing
		, Just (Param "--no-filters")
		]

hashObjectStop :: HashObjectHandle -> IO ()
hashObjectStop (HashObjectHandle h _ _) = CoProcess.stop h

{- Injects a file into git, returning the Sha of the object. -}
hashFile :: HashObjectHandle -> OsPath -> IO Sha
hashFile hdl@(HashObjectHandle h _ _) file = do
	-- git hash-object chdirs to the top of the repository on
	-- start, so if the filename is relative, it will
	-- not work. This seems likely to be a git bug.
	-- So, make the filename absolute, which will work now
	-- and also if git's behavior later changes.
	file' <- absPath file
	if newline `OS.elem` file' || carriagereturn `OS.elem` file
		then hashFile' hdl file
		else CoProcess.query h (send (fromOsPath file')) receive
  where
	send file' to = S8.hPutStrLn to file'
	receive from = getSha "hash-object" $ S8.hGetLine from
	newline = unsafeFromChar '\n'
	-- git strips carriage return from the end of a line, out of some
	-- misplaced desire to support windows, so also use the newline
	-- fallback for those.
	carriagereturn = unsafeFromChar '\r'

{- Runs git hash-object once per call, rather than using a running
 - one, so is slower. But, is able to handle newlines in the filepath,
 - which --stdin-paths cannot. -}
hashFile' :: HashObjectHandle -> OsPath -> IO Sha
hashFile' (HashObjectHandle _ repo ps) file = getSha "hash-object" $
	pipeReadStrict (ps ++ [File (fromOsPath file)]) repo

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
hashBlob h b = withTmpFile (literalOsPath "hash") $ \tmp tmph -> do
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

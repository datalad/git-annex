{- git hash-object interface
 -
 - Copyright 2011-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
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

type HashObjectHandle = CoProcess.CoProcessHandle

hashObjectStart :: Repo -> IO HashObjectHandle
hashObjectStart = gitCoProcessStart True
	[ Param "hash-object"
	, Param "-w"
	, Param "--stdin-paths"
	, Param "--no-filters"
	]

hashObjectStop :: HashObjectHandle -> IO ()
hashObjectStop = CoProcess.stop

{- Injects a file into git, returning the Sha of the object. -}
hashFile :: HashObjectHandle -> FilePath -> IO Sha
hashFile h file = CoProcess.query h send receive
  where
	send to = hPutStrLn to =<< absPath file
	receive from = getSha "hash-object" $ hGetLine from

{- Injects a blob into git. Unfortunately, the current git-hash-object
 - interface does not allow batch hashing without using temp files. -}
hashBlob :: HashObjectHandle -> String -> IO Sha
hashBlob h s = withTmpFile "hash" $ \tmp tmph -> do
#ifdef mingw32_HOST_OS
	hSetNewlineMode tmph noNewlineTranslation
#endif
	hPutStr tmph s
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
	params = [subcmd, "-t", show objtype, "-w", "--stdin", "--no-filters"]

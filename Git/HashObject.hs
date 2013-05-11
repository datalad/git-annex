{- git hash-object interface
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
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

type HashObjectHandle = CoProcess.CoProcessHandle

hashObjectStart :: Repo -> IO HashObjectHandle
hashObjectStart = gitCoProcessStart
	[ Param "hash-object"
	, Param "-w"
	, Param "--stdin-paths"
	]

hashObjectStop :: HashObjectHandle -> IO ()
hashObjectStop = CoProcess.stop

{- Injects a file into git, returning the Sha of the object. -}
hashFile :: HashObjectHandle -> FilePath -> IO Sha
hashFile h file = CoProcess.query h send receive
  where
	send to = do
		fileEncoding to
#ifdef __WINDOWS__
		hSetNewlineMode to noNewlineTranslation
#endif
		hPutStrLn to file
	receive from = getSha "hash-object" $ do
#ifdef __WINDOWS__
		hSetNewlineMode from noNewlineTranslation
#endif
		hGetLine from

{- Injects some content into git, returning its Sha. -}
hashObject :: ObjectType -> String -> Repo -> IO Sha
hashObject objtype content repo = getSha subcmd $ do
	s <- pipeWriteRead (map Param params) content repo
	return s
  where
	subcmd = "hash-object"
	params = [subcmd, "-t", show objtype, "-w", "--stdin"]

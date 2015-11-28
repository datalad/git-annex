{- git check-ignore interface
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.CheckIgnore (
	CheckIgnoreHandle,
	checkIgnoreStart,
	checkIgnoreStop,
	checkIgnored
) where

import Common
import Git
import Git.Command
import qualified Git.Version
import qualified Utility.CoProcess as CoProcess

import System.IO.Error

type CheckIgnoreHandle = CoProcess.CoProcessHandle

{- Starts git check-ignore running, and returns a handle.
 -
 - This relies on git check-ignore --non-matching -v outputting
 - lines for both matching an non-matching files. Also relies on
 - GIT_FLUSH behavior flushing the output buffer when git check-ignore
 - is piping to us.
 -
 - The first version of git to support what we need is 1.8.4.
 - Nothing is returned if an older git is installed.
 -
 - check-ignore does not support --literal-pathspecs, so remove that
 - from the gitGlobalOpts if set.
 -}
checkIgnoreStart :: Repo -> IO (Maybe CheckIgnoreHandle)
checkIgnoreStart repo = ifM supportedGitVersion
	( Just <$> (CoProcess.rawMode =<< gitCoProcessStart True params repo')
	, return Nothing
	)
  where
	params =
		[ Param "check-ignore" 
		, Param "-z"
		, Param "--stdin"
		, Param "--verbose"
		, Param "--non-matching"
		]
	repo' = repo { gitGlobalOpts = filter (not . pathspecs) (gitGlobalOpts repo) }
	pathspecs (Param "--literal-pathspecs") = True
	pathspecs _ = False

supportedGitVersion :: IO Bool
supportedGitVersion = do
	v <- Git.Version.installed
	return $ v >= Git.Version.normalize "1.8.4"

{- For some reason, check-ignore --batch always exits nonzero, 
 - so ignore any error. -}
checkIgnoreStop :: CheckIgnoreHandle -> IO ()
checkIgnoreStop = void . tryIO . CoProcess.stop

{- Returns True if a file is ignored. -}
checkIgnored :: CheckIgnoreHandle -> FilePath -> IO Bool
checkIgnored h file = CoProcess.query h send (receive "")
  where
	send to = do
		hPutStr to $ file ++ "\0"
		hFlush to
	receive c from = do
		s <- hGetSomeString from 1024
		if null s
			then eofError
			else do
				let v = c ++ s
				maybe (receive v from) return (parse v)
	parse s = case segment (== '\0') s of
		(_source:_line:pattern:_pathname:_eol:[]) -> Just $ not $ null pattern
		_ -> Nothing
	eofError = ioError $ mkIOError userErrorType "git cat-file EOF" Nothing Nothing

{- git-annex file copying with rsync
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.RsyncFile where

import Data.String.Utils

import Utility

{- Generates parameters to make rsync use a specified command as its remote
 - shell. -}
rsyncShell :: [CommandParam] -> [CommandParam]
rsyncShell command = [Param "-e", Param $ unwords $ map escape (toCommand command)]
	where
		{- rsync requires some weird, non-shell like quoting in
                 - here. A doubled single quote inside the single quoted
                 - string is a single quote. -}
		escape s = "'" ++  join "''" (split "'" s) ++ "'"

{- Runs rsync in server mode to send a file, and exits. -}
rsyncServerSend :: FilePath -> IO ()
rsyncServerSend file = rsyncExec $
	rsyncServerParams ++ [Param "--sender", File file]

{- Runs rsync in server mode to receive a file. -}
rsyncServerReceive :: FilePath -> IO Bool
rsyncServerReceive file = rsync $ rsyncServerParams ++ [File file]

rsyncServerParams :: [CommandParam]
rsyncServerParams =
	[ Param "--server"
	-- preserve permissions
	, Param "-p"
	-- allow resuming of transfers of big files
	, Param "--inplace"
	-- other options rsync normally uses in server mode
	, Params "-e.Lsf ."
	]

rsync :: [CommandParam] -> IO Bool
rsync = boolSystem "rsync"

rsyncExec :: [CommandParam] -> IO ()
rsyncExec params = executeFile "rsync" True (toCommand params) Nothing

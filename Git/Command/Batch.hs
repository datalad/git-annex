{- running batch git commands
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Command.Batch where

import Common
import Git
import Git.Command
import Utility.Batch

{- Runs git in batch mode. -}
run :: BatchCommandMaker -> [CommandParam] -> Repo -> IO Bool
run batchmaker params repo = assertLocal repo $ do
	let (cmd, params') = batchmaker ("git", gitCommandLine params repo)
	boolSystemEnv cmd params' (gitEnv repo)

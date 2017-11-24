{- git reflog interface
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.RefLog where

import Common
import Git
import Git.Command
import Git.Sha

{- Gets the reflog for a given branch. -}
get :: Branch -> Repo -> IO [Sha]
get b = getMulti [b]

{- Gets reflogs for multiple branches. -}
getMulti :: [Branch] -> Repo -> IO [Sha]
getMulti bs = get' (map (Param . fromRef) bs)

get' :: [CommandParam] -> Repo -> IO [Sha]
get' ps = mapMaybe extractSha . lines <$$> pipeReadStrict ps'
  where
	ps' = catMaybes
		[ Just $ Param "log"
		, Just $ Param "-g"
		, Just $ Param "--format=%H"
		] ++ ps

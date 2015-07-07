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
get b = get' [] (Just b)

{- Gets all reflogs for all branches. -}
getAll :: Repo -> IO [Sha]
getAll = get' [Param "--all"] Nothing

get' :: [CommandParam] -> Maybe Branch -> Repo -> IO [Sha]
get' ps b = mapMaybe extractSha . lines <$$> pipeReadStrict ps'
  where
	ps' = catMaybes
		[ Just $ Param "log"
		, Just $ Param "-g"
		, Just $ Param "--format=%H"
		, Param . fromRef <$> b
		] ++ ps

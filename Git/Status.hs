{- git status interface
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Status where

import Common
import Git
import Git.Command
import Git.FilePath

data Status
	= Modified TopFilePath
	| Deleted TopFilePath
	| Added TopFilePath
	| Renamed TopFilePath TopFilePath
	| TypeChanged TopFilePath
	| Untracked TopFilePath

statusChar :: Status -> Char
statusChar (Modified _) = 'M'
statusChar (Deleted _) = 'D'
statusChar (Added _) = 'A'
statusChar (Renamed _ _) = 'R'
statusChar (TypeChanged _) = 'T'
statusChar (Untracked _) = '?'

statusFile :: Status -> TopFilePath
statusFile (Modified f) = f
statusFile (Deleted f) = f
statusFile (Added f) = f
statusFile (Renamed _oldf newf) = newf
statusFile (TypeChanged f) = f
statusFile (Untracked f) = f

parseStatusZ :: [String] -> [Status]
parseStatusZ = go []
  where
	go c [] = reverse c
	go c (x:xs) = case x of
		(sindex:sworktree:' ':f) -> 
			-- Look at both the index and worktree status,
			-- preferring worktree.
			case cparse sworktree <|> cparse sindex of
				Just mks -> go (mks (asTopFilePath f) : c) xs
				Nothing -> if sindex == 'R'
					-- In -z mode, the name the
					-- file was renamed to comes
					-- first, and the next component
					-- is the old filename.
					then case xs of
						(oldf:xs') -> go (Renamed (asTopFilePath oldf) (asTopFilePath f) : c) xs'
						_ -> go c []
					else go c xs
		_ -> go c xs

	cparse 'M' = Just Modified
	cparse 'A' = Just Added
	cparse 'D' = Just Deleted
	cparse 'T' = Just TypeChanged
	cparse '?' = Just Untracked
	cparse _ = Nothing

getStatus :: [CommandParam] -> [FilePath] -> Repo -> IO ([Status], IO Bool)
getStatus ps fs r = do
	(ls, cleanup) <- pipeNullSplit ps' r
	return (parseStatusZ ls, cleanup)
  where
	ps' = concat
		[ [Param "status"]
		, ps
		, [ Param "-uall" , Param "-z"]
		, map File fs
		]

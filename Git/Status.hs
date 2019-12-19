{- git status interface
 -
 - Copyright 2015-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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

data StagedUnstaged a = StagedUnstaged
	{ staged :: Maybe a
	, unstaged :: Maybe a
	}

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

parseStatusZ :: [String] -> [StagedUnstaged Status]
parseStatusZ = go []
  where
	go c [] = reverse c
	go c (x:xs) = case x of
		(sstaged:sunstaged:' ':f) -> 
			case (cparse sstaged f xs, cparse sunstaged f xs) of
				((vstaged, xs1), (vunstaged, xs2)) ->
					let v = StagedUnstaged
						{ staged = vstaged
						, unstaged = vunstaged
						}
					    xs' = fromMaybe xs (xs1 <|> xs2)
					in go (v : c) xs'
		_ -> go c xs

	cparse 'M' f _ = (Just (Modified (asTopFilePath (toRawFilePath f))), Nothing)
	cparse 'A' f _ = (Just (Added (asTopFilePath (toRawFilePath f))), Nothing)
	cparse 'D' f _ = (Just (Deleted (asTopFilePath (toRawFilePath f))), Nothing)
	cparse 'T' f _ = (Just (TypeChanged (asTopFilePath (toRawFilePath f))), Nothing)
	cparse '?' f _ = (Just (Untracked (asTopFilePath (toRawFilePath f))), Nothing)
	cparse 'R' f (oldf:xs) =
		(Just (Renamed (asTopFilePath (toRawFilePath oldf)) (asTopFilePath (toRawFilePath f))), Just xs)
	cparse _ _ _ = (Nothing, Nothing)

getStatus :: [CommandParam] -> [FilePath] -> Repo -> IO ([StagedUnstaged Status], IO Bool)
getStatus ps fs r = do
	(ls, cleanup) <- pipeNullSplit ps' r
	return (parseStatusZ (map decodeBL ls), cleanup)
  where
	ps' = concat
		[ [Param "status"]
		, ps
		, [ Param "-uall" , Param "-z"]
		, map File fs
		]

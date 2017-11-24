{- This is not the same as git's fetch/push refspecs.
 - 
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.RefSpec where

import Common
import Utility.Glob
import Git.Types

import Data.Either

type RefSpec = [RefSpecPart]

data RefSpecPart
	= AddRef Ref
	| AddMatching Glob
	| AddRefLog
	| RemoveMatching Glob

allRefSpec :: RefSpec
allRefSpec = [AddMatching $ compileGlob "*" CaseSensative]

parseRefSpec :: String -> Either String RefSpec
parseRefSpec v = case partitionEithers (map mk $ splitc ':' v) of
	([],refspec) -> Right refspec
	(e:_,_) -> Left e
  where
	mk ('+':s)
		| any (`elem` s) "*?" =
			Right $ AddMatching $ compileGlob s CaseSensative
		| otherwise = Right $ AddRef $ Ref s
	mk ('-':s) = Right $ RemoveMatching $ compileGlob s CaseSensative
	mk "reflog" = Right AddRefLog
	mk s = Left $ "bad refspec item \"" ++ s ++ "\" (expected + or - prefix)"

applyRefSpec :: Monad m => RefSpec -> [Ref] -> m [Sha] -> m [Ref]
applyRefSpec refspec rs getreflog = go [] refspec
  where
	go c [] = return (reverse c)
	go c (AddRef r : rest) = go (r:c) rest
	go c (AddMatching g : rest) =
		let add = filter (matchGlob g . fromRef) rs
		in go (add ++ c) rest
	go c (AddRefLog : rest) = do
		reflog <- getreflog
		go (reflog ++ c) rest
	go c (RemoveMatching g : rest) = 
		go (filter (not . matchGlob g . fromRef) c) rest

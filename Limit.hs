{- user-specified limits on files to act on
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Limit where

import Text.Regex.PCRE.Light.Char8
import System.Path.WildMatch
import Control.Monad (filterM)
import Data.Maybe

import Annex
import qualified Utility.Matcher

type Limit = Utility.Matcher.Token (FilePath -> Annex Bool)

{- Filter out files not matching user-specified limits. -}
filterFiles :: [FilePath] -> Annex [FilePath]
filterFiles l = do
	matcher <- getMatcher
	filterM (Utility.Matcher.matchM matcher) l

{- Gets a matcher for the user-specified limits. The matcher is cached for
 - speed; once it's obtained the user-specified limits can't change. -}
getMatcher :: Annex (Utility.Matcher.Matcher (FilePath -> Annex Bool))
getMatcher = do
	m <- Annex.getState Annex.limit
	case m of
		Right r -> return r
		Left l -> do
			let matcher = Utility.Matcher.generate (reverse l)
			Annex.changeState $ \s -> s { Annex.limit = Right matcher }
			return matcher

{- Adds something to the limit list. -}
add :: Limit -> Annex ()
add l = Annex.changeState $ \s -> s { Annex.limit = append $ Annex.limit s }
	where
		append (Left ls) = Left $ l:ls
		append _ = error "internal"

{- Adds a new limit. -}
addl :: (FilePath -> Annex Bool) -> Annex ()
addl = add . Utility.Matcher.Operation

{- Adds a new token. -}
addt :: String -> Annex ()
addt = add . Utility.Matcher.Token

{- Add a limit to skip files that do not match the glob. -}
exclude :: String -> Annex ()
exclude glob = addl $ return . notExcluded
	where
		notExcluded f = isNothing $ match cregex f []
		cregex = compile regex []
		regex = '^':wildToRegex glob

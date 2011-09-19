{- user-specified limits on files to act on
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Limit where

import Text.Regex.PCRE.Light.Char8
import System.Path.WildMatch
import Control.Applicative
import Data.Maybe

import Annex
import qualified Utility.Matcher
import qualified Remote
import qualified Backend
import LocationLog
import Utility
import Content

type Limit = Utility.Matcher.Token (FilePath -> Annex Bool)

{- Checks if there are user-specified limits. -}
limited :: Annex Bool
limited = (not . Utility.Matcher.matchesAny) <$> getMatcher'

{- Gets a matcher for the user-specified limits. The matcher is cached for
 - speed; once it's obtained the user-specified limits can't change. -}
getMatcher :: Annex (FilePath -> Annex Bool)
getMatcher = Utility.Matcher.matchM <$> getMatcher'

getMatcher' :: Annex (Utility.Matcher.Matcher (FilePath -> Annex Bool))
getMatcher' = do
	m <- Annex.getState Annex.limit
	case m of
		Right r -> return r
		Left l -> do
			let matcher = Utility.Matcher.generate (reverse l)
			Annex.changeState $ \s -> s { Annex.limit = Right matcher }
			return matcher

{- Adds something to the limit list, which is built up reversed. -}
add :: Limit -> Annex ()
add l = Annex.changeState $ \s -> s { Annex.limit = append $ Annex.limit s }
	where
		append (Left ls) = Left $ l:ls
		append _ = error "internal"

{- Adds a new limit. -}
addlimit :: (FilePath -> Annex Bool) -> Annex ()
addlimit = add . Utility.Matcher.Operation

{- Adds a new token. -}
token :: String -> Annex ()
token = add . Utility.Matcher.Token

{- Add a limit to skip files that do not match the glob. -}
addExclude :: String -> Annex ()
addExclude glob = addlimit $ return . notExcluded
	where
		notExcluded f = isNothing $ match cregex f []
		cregex = compile regex []
		regex = '^':wildToRegex glob

{- Adds a limit to skip files not believed to be present
 - on a specfied repository. -}
addIn :: String -> Annex ()
addIn name = do
	u <- Remote.nameToUUID name
	addlimit $ if name == "." then check local else check (remote u)
	where
		check a f = Backend.lookupFile f >>= handle a
		handle _ Nothing = return False
		handle a (Just (key, _)) = a key
		remote u key = do
			us <- keyLocations key
			return $ u `elem` us
		local key = inAnnex key

{- Adds a limit to skip files not believed to have the specified number
 - of copies. -}
addCopies :: String -> Annex ()
addCopies num = do
	case readMaybe num :: Maybe Int of
		Nothing -> error "bad number for --copies"
		Just n -> addlimit $ check n
	where
		check n f = Backend.lookupFile f >>= handle n
		handle _ Nothing = return False
		handle n (Just (key, _)) = do
			us <- keyLocations key
			return $ length us >= n

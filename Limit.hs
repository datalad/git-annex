{- user-specified limits on files to act on
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Limit where

import Text.Regex.PCRE.Light.Char8
import System.Path.WildMatch

import Common.Annex
import qualified Annex
import qualified Utility.Matcher
import qualified Remote
import qualified Backend
import Annex.Content

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
add l = Annex.changeState $ \s -> s { Annex.limit = prepend $ Annex.limit s }
	where
		prepend (Left ls) = Left $ l:ls
		prepend _ = error "internal"

{- Adds a new token. -}
addToken :: String -> Annex ()
addToken = add . Utility.Matcher.token

{- Adds a new limit. -}
addLimit :: (FilePath -> Annex Bool) -> Annex ()
addLimit = add . Utility.Matcher.Operation

{- Add a limit to skip files that do not match the glob. -}
addInclude :: String -> Annex ()
addInclude glob = addLimit $ return . matchglob glob

{- Add a limit to skip files that match the glob. -}
addExclude :: String -> Annex ()
addExclude glob = addLimit $ return . not . matchglob glob

matchglob :: String -> FilePath -> Bool
matchglob glob f = isJust $ match cregex f []
	where
		cregex = compile regex []
		regex = '^':wildToRegex glob

{- Adds a limit to skip files not believed to be present
 - in a specfied repository. -}
addIn :: String -> Annex ()
addIn name = addLimit $ check $ if name == "." then inAnnex else inremote
	where
		check a = Backend.lookupFile >=> handle a
		handle _ Nothing = return False
		handle a (Just (key, _)) = a key
		inremote key = do
			u <- Remote.nameToUUID name
			us <- Remote.keyLocations key
			return $ u `elem` us

{- Adds a limit to skip files not believed to have the specified number
 - of copies. -}
addCopies :: String -> Annex ()
addCopies num =
	case readish num :: Maybe Int of
		Nothing -> error "bad number for --copies"
		Just n -> addLimit $ check n
	where
		check n = Backend.lookupFile >=> handle n
		handle _ Nothing = return False
		handle n (Just (key, _)) = do
			us <- Remote.keyLocations key
			return $ length us >= n

{- Adds a limit to skip files not using a specified key-value backend. -}
addInBackend :: String -> Annex ()
addInBackend name = addLimit $ Backend.lookupFile >=> check
	where
		wanted = Backend.lookupBackendName name
		check = return . maybe False ((==) wanted . snd)

{- user-specified limits on files to act on
 -
 - Copyright 2011,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Limit where

import Text.Regex.PCRE.Light.Char8
import System.Path.WildMatch
import Data.Time.Clock.POSIX
import qualified Data.Set as S

import Common.Annex
import qualified Annex
import qualified Utility.Matcher
import qualified Remote
import qualified Backend
import Annex.Content
import Logs.Trust
import Types.TrustLevel
import Logs.Group
import Utility.HumanTime

type Limit = Utility.Matcher.Token (FilePath -> Annex Bool)
type MkLimit = String -> Either String (FilePath -> Annex Bool)

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
addLimit :: Either String (FilePath -> Annex Bool) -> Annex ()
addLimit = either error (add . Utility.Matcher.Operation)

{- Add a limit to skip files that do not match the glob. -}
addInclude :: String -> Annex ()
addInclude = addLimit . limitInclude

limitInclude :: MkLimit
limitInclude glob = Right $ return . matchglob glob

{- Add a limit to skip files that match the glob. -}
addExclude :: String -> Annex ()
addExclude = addLimit . limitExclude

limitExclude :: MkLimit
limitExclude glob = Right $ return . not . matchglob glob

matchglob :: String -> FilePath -> Bool
matchglob glob f = isJust $ match cregex f []
	where
		cregex = compile regex []
		regex = '^':wildToRegex glob

{- Adds a limit to skip files not believed to be present
 - in a specfied repository. -}
addIn :: String -> Annex ()
addIn = addLimit . limitIn

limitIn :: MkLimit
limitIn name = Right $ check $ if name == "." then inAnnex else inremote
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
addCopies = addLimit . limitCopies

limitCopies :: MkLimit
limitCopies want = case split ":" want of
	[v, n] -> case readTrustLevel v of
		Just trust -> go n $ checktrust trust
		Nothing -> go n $ checkgroup v
	[n] -> go n $ const $ return True
	_ -> Left "bad value for copies"
	where
		go num good = case readish num of
			Nothing -> Left "bad number for copies"
			Just n -> Right $ check n good
		check n good = Backend.lookupFile >=> handle n good
		handle _ _ Nothing = return False
		handle n good (Just (key, _)) = do
			us <- filterM good =<< Remote.keyLocations key
			return $ length us >= n
		checktrust t u = (== t) <$> lookupTrust u
		checkgroup g u = S.member g <$> lookupGroups u

{- Adds a limit to skip files not using a specified key-value backend. -}
addInBackend :: String -> Annex ()
addInBackend = addLimit . limitInBackend

limitInBackend :: MkLimit
limitInBackend name = Right $ Backend.lookupFile >=> check
	where
		wanted = Backend.lookupBackendName name
		check = return . maybe False ((==) wanted . snd)

addTimeLimit :: String -> Annex ()
addTimeLimit s = do
	let seconds = fromMaybe (error "bad time-limit") $ parseDuration s
	start <- liftIO getPOSIXTime
	let cutoff = start + seconds
	addLimit $ Right $ const $ do
		now <- liftIO getPOSIXTime
		if now > cutoff
			then do
				warning $ "Time limit (" ++ s ++ ") reached!"
				liftIO $ exitWith $ ExitFailure 101
			else return True

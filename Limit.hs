{- user-specified limits on files to act on
 -
 - Copyright 2011-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Limit where

import Common.Annex
import qualified Annex
import qualified Utility.Matcher
import qualified Remote
import qualified Backend
import Annex.Content
import Annex.UUID
import Logs.Trust
import Config.NumCopies
import Types.TrustLevel
import Types.Key
import Types.Group
import Types.FileMatcher
import Types.Limit
import Types.MetaData
import Logs.MetaData
import Logs.Group
import Logs.Unused
import Logs.Location
import Git.Types (RefDate(..))
import Utility.HumanTime
import Utility.DataUnits

import Data.Time.Clock.POSIX
import qualified Data.Set as S
import qualified Data.Map as M
import System.Path.WildMatch

#ifdef WITH_TDFA
import Text.Regex.TDFA
import Text.Regex.TDFA.String
#endif

{- Checks if there are user-specified limits. -}
limited :: Annex Bool
limited = (not . Utility.Matcher.isEmpty) <$> getMatcher'

{- Gets a matcher for the user-specified limits. The matcher is cached for
 - speed; once it's obtained the user-specified limits can't change. -}
getMatcher :: Annex (MatchInfo -> Annex Bool)
getMatcher = Utility.Matcher.matchM <$> getMatcher'

getMatcher' :: Annex (Utility.Matcher.Matcher (MatchInfo -> Annex Bool))
getMatcher' = do
	m <- Annex.getState Annex.limit
	case m of
		Right r -> return r
		Left l -> do
			let matcher = Utility.Matcher.generate (reverse l)
			Annex.changeState $ \s ->
				s { Annex.limit = Right matcher }
			return matcher

{- Adds something to the limit list, which is built up reversed. -}
add :: Utility.Matcher.Token (MatchInfo -> Annex Bool) -> Annex ()
add l = Annex.changeState $ \s -> s { Annex.limit = prepend $ Annex.limit s }
  where
	prepend (Left ls) = Left $ l:ls
	prepend _ = error "internal"

{- Adds a new token. -}
addToken :: String -> Annex ()
addToken = add . Utility.Matcher.token

{- Adds a new limit. -}
addLimit :: Either String MatchFiles -> Annex ()
addLimit = either error (\l -> add $ Utility.Matcher.Operation $ l S.empty)

{- Add a limit to skip files that do not match the glob. -}
addInclude :: String -> Annex ()
addInclude = addLimit . limitInclude

limitInclude :: MkLimit
limitInclude glob = Right $ const $ return . matchglob glob

{- Add a limit to skip files that match the glob. -}
addExclude :: String -> Annex ()
addExclude = addLimit . limitExclude

limitExclude :: MkLimit
limitExclude glob = Right $ const $ return . not . matchglob glob

{- Could just use wildCheckCase, but this way the regex is only compiled
 - once. Also, we use regex-TDFA when available, because it's less buggy
 - in its support of non-unicode characters. -}
matchglob :: String -> MatchInfo -> Bool
matchglob glob (MatchingFile fi) =
#ifdef WITH_TDFA
	case cregex of
		Right r -> case execute r (matchFile fi) of
			Right (Just _) -> True
			_ -> False
		Left _ -> error $ "failed to compile regex: " ++ regex
  where
	cregex = compile defaultCompOpt defaultExecOpt regex
	regex = '^':wildToRegex glob
#else
	wildCheckCase glob (matchFile fi)
#endif
matchglob _ (MatchingKey _) = False

{- Adds a limit to skip files not believed to be present
 - in a specfied repository. Optionally on a prior date. -}
addIn :: String -> Annex ()
addIn = addLimit . limitIn

limitIn :: MkLimit
limitIn s = Right $ \notpresent -> checkKey $ \key ->
	if name == "."
		then if null date
			then inhere notpresent key
			else inuuid notpresent key =<< getUUID
		else inuuid notpresent key =<< Remote.nameToUUID name
  where
	(name, date) = separate (== '@') s
	inuuid notpresent key u
		| null date = do
			us <- Remote.keyLocations key
			return $ u `elem` us && u `S.notMember` notpresent
		| otherwise = do
			us <- loggedLocationsHistorical (RefDate date) key
			return $ u `elem` us
	inhere notpresent key
		| S.null notpresent = inAnnex key
		| otherwise = do
			u <- getUUID
			if u `S.member` notpresent
				then return False
				else inAnnex key

{- Limit to content that is currently present on a uuid. -}
limitPresent :: Maybe UUID -> MkLimit
limitPresent u _ = Right $ const $ checkKey $ \key -> do
	hereu <- getUUID
	if u == Just hereu || isNothing u
		then inAnnex key
		else do
			us <- Remote.keyLocations key
			return $ maybe False (`elem` us) u

{- Limit to content that is in a directory, anywhere in the repository tree -}
limitInDir :: FilePath -> MkLimit
limitInDir dir = const $ Right $ const go
  where
	go (MatchingFile fi) = return $ elem dir $ splitPath $ takeDirectory $ matchFile fi
	go (MatchingKey _) = return False

{- Adds a limit to skip files not believed to have the specified number
 - of copies. -}
addCopies :: String -> Annex ()
addCopies = addLimit . limitCopies

limitCopies :: MkLimit
limitCopies want = case split ":" want of
	[v, n] -> case parsetrustspec v of
		Just checker -> go n $ checktrust checker
		Nothing -> go n $ checkgroup v
	[n] -> go n $ const $ return True
	_ -> Left "bad value for copies"
  where
	go num good = case readish num of
		Nothing -> Left "bad number for copies"
		Just n -> Right $ \notpresent -> checkKey $
			handle n good notpresent
	handle n good notpresent key = do
		us <- filter (`S.notMember` notpresent)
			<$> (filterM good =<< Remote.keyLocations key)
		return $ length us >= n
	checktrust checker u = checker <$> lookupTrust u
	checkgroup g u = S.member g <$> lookupGroups u
	parsetrustspec s
		| "+" `isSuffixOf` s = (>=) <$> readTrustLevel (beginning s)
		| otherwise = (==) <$> readTrustLevel s

{- Adds a limit to match files that need more copies made. -}
addLackingCopies :: Bool -> String -> Annex ()
addLackingCopies approx = addLimit . limitLackingCopies approx

limitLackingCopies :: Bool -> MkLimit
limitLackingCopies approx want = case readish want of
	Just needed -> Right $ \notpresent mi -> flip checkKey mi $
		handle mi needed notpresent
	Nothing -> Left "bad value for number of lacking copies"
  where
	handle mi needed notpresent key = do
		NumCopies numcopies <- if approx
			then approxNumCopies
			else case mi of
				MatchingKey _ -> approxNumCopies
				MatchingFile fi -> getGlobalFileNumCopies $ matchFile fi
		us <- filter (`S.notMember` notpresent)
			<$> (trustExclude UnTrusted =<< Remote.keyLocations key)
		return $ numcopies - length us >= needed
	approxNumCopies = fromMaybe defaultNumCopies <$> getGlobalNumCopies

{- Match keys that are unused.
 - 
 - This has a nice optimisation: When a file exists,
 - its key is obviously not unused.
 -}
limitUnused :: MatchFiles
limitUnused _ (MatchingFile _) = return False
limitUnused _ (MatchingKey k) = S.member k <$> unusedKeys

{- Adds a limit to skip files not believed to be present in all
 - repositories in the specified group. -}
addInAllGroup :: String -> Annex ()
addInAllGroup groupname = do
	m <- groupMap
	addLimit $ limitInAllGroup m groupname

limitInAllGroup :: GroupMap -> MkLimit
limitInAllGroup m groupname
	| S.null want = Right $ const $ const $ return True
	| otherwise = Right $ \notpresent -> checkKey $ check notpresent
  where
	want = fromMaybe S.empty $ M.lookup groupname $ uuidsByGroup m
	check notpresent key
		-- optimisation: Check if a wanted uuid is notpresent.
		| not (S.null (S.intersection want notpresent))	= return False
		| otherwise = do
			present <- S.fromList <$> Remote.keyLocations key
			return $ S.null $ want `S.difference` present

{- Adds a limit to skip files not using a specified key-value backend. -}
addInBackend :: String -> Annex ()
addInBackend = addLimit . limitInBackend

limitInBackend :: MkLimit
limitInBackend name = Right $ const $ checkKey check
  where
	check key = pure $ keyBackendName key == name

{- Adds a limit to skip files that are too large or too small -}
addLargerThan :: String -> Annex ()
addLargerThan = addLimit . limitSize (>)

addSmallerThan :: String -> Annex ()
addSmallerThan = addLimit . limitSize (<)

limitSize :: (Maybe Integer -> Maybe Integer -> Bool) -> MkLimit
limitSize vs s = case readSize dataUnits s of
	Nothing -> Left "bad size"
	Just sz -> Right $ go sz
  where
  	go sz _ (MatchingFile fi) = lookupFile fi >>= check fi sz
	go sz _ (MatchingKey key) = checkkey sz key
	checkkey sz key = return $ keySize key `vs` Just sz
	check _ sz (Just (key, _)) = checkkey sz key
	check fi sz Nothing = do
		filesize <- liftIO $ catchMaybeIO $
			fromIntegral . fileSize
				<$> getFileStatus (relFile fi)
		return $ filesize `vs` Just sz

addMetaData :: String -> Annex ()
addMetaData = addLimit . limitMetaData

limitMetaData :: MkLimit
limitMetaData s = case parseMetaData s of
	Left e -> Left e
	Right (f, v) -> Right $ const $ checkKey (check f v)
  where
  	check f v k = S.member v . metaDataValues f <$> getCurrentMetaData k

addTimeLimit :: String -> Annex ()
addTimeLimit s = do
	let seconds = maybe (error "bad time-limit") durationToPOSIXTime $
		parseDuration s
	start <- liftIO getPOSIXTime
	let cutoff = start + seconds
	addLimit $ Right $ const $ const $ do
		now <- liftIO getPOSIXTime
		if now > cutoff
			then do
				warning $ "Time limit (" ++ s ++ ") reached!"
				liftIO $ exitWith $ ExitFailure 101
			else return True

lookupFile :: FileInfo -> Annex (Maybe (Key, Backend))
lookupFile = Backend.lookupFile . relFile

lookupFileKey :: FileInfo -> Annex (Maybe Key)
lookupFileKey = (fst <$>) <$$> Backend.lookupFile . relFile

checkKey :: (Key -> Annex Bool) -> MatchInfo -> Annex Bool
checkKey a (MatchingFile fi) = lookupFileKey fi >>= maybe (return False) a
checkKey a (MatchingKey k) = a k

{- user-specified limits on files to act on
 -
 - Copyright 2011-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Limit where

import Annex.Common
import qualified Annex
import qualified Utility.Matcher
import qualified Remote
import Annex.Content
import Annex.WorkTree
import Annex.Action
import Annex.UUID
import Logs.Trust
import Annex.NumCopies
import Types.TrustLevel
import Types.Group
import Types.FileMatcher
import Types.MetaData
import Annex.MetaData
import Logs.MetaData
import Logs.Group
import Logs.Unused
import Logs.Location
import Git.Types (RefDate(..))
import Utility.Glob
import Utility.HumanTime
import Utility.DataUnits

#ifdef WITH_MAGICMIME
import Magic
#endif

import Data.Time.Clock.POSIX
import qualified Data.Set as S
import qualified Data.Map as M

{- Checks if there are user-specified limits. -}
limited :: Annex Bool
limited = (not . Utility.Matcher.isEmpty) <$> getMatcher'

{- Gets a matcher for the user-specified limits. The matcher is cached for
 - speed; once it's obtained the user-specified limits can't change. -}
getMatcher :: Annex (MatchInfo -> Annex Bool)
getMatcher = Utility.Matcher.matchM <$> getMatcher'

getMatcher' :: Annex (Utility.Matcher.Matcher (MatchInfo -> Annex Bool))
getMatcher' = go =<< Annex.getState Annex.limit
  where
	go (CompleteMatcher matcher) = return matcher
	go (BuildingMatcher l) = do
		let matcher = Utility.Matcher.generate (reverse l)
		Annex.changeState $ \s ->
			s { Annex.limit = CompleteMatcher matcher }
		return matcher

{- Adds something to the limit list, which is built up reversed. -}
add :: Utility.Matcher.Token (MatchInfo -> Annex Bool) -> Annex ()
add l = Annex.changeState $ \s -> s { Annex.limit = prepend $ Annex.limit s }
  where
	prepend (BuildingMatcher ls) = BuildingMatcher $ l:ls
	prepend _ = error "internal"

{- Adds a new token. -}
addToken :: String -> Annex ()
addToken = add . Utility.Matcher.token

{- Adds a new limit. -}
addLimit :: Either String (MatchFiles Annex) -> Annex ()
addLimit = either error (\l -> add $ Utility.Matcher.Operation $ l S.empty)

{- Add a limit to skip files that do not match the glob. -}
addInclude :: String -> Annex ()
addInclude = addLimit . limitInclude

limitInclude :: MkLimit Annex
limitInclude glob = Right $ const $ matchGlobFile glob

{- Add a limit to skip files that match the glob. -}
addExclude :: String -> Annex ()
addExclude = addLimit . limitExclude

limitExclude :: MkLimit Annex
limitExclude glob = Right $ const $ not <$$> matchGlobFile glob

matchGlobFile :: String -> MatchInfo -> Annex Bool
matchGlobFile glob = go
  where
	cglob = compileGlob glob CaseSensative -- memoized
	go (MatchingKey _) = pure False
	go (MatchingFile fi) = pure $ matchGlob cglob (matchFile fi)
	go (MatchingInfo af _ _ _) = matchGlob cglob <$> getInfo af

#ifdef WITH_MAGICMIME
matchMagic :: Maybe Magic -> MkLimit Annex
matchMagic (Just magic) glob = Right $ const go
  where
 	cglob = compileGlob glob CaseSensative -- memoized
	go (MatchingKey _) = pure False
	go (MatchingFile fi) = liftIO $ catchBoolIO $
		matchGlob cglob <$> magicFile magic (matchFile fi)
	go (MatchingInfo _ _ _ mimeval) = matchGlob cglob <$> getInfo mimeval
matchMagic Nothing _ = Left "unable to load magic database; \"mimetype\" cannot be used"
#endif

{- Adds a limit to skip files not believed to be present
 - in a specfied repository. Optionally on a prior date. -}
addIn :: String -> Annex ()
addIn s = addLimit =<< mk
  where
	(name, date) = separate (== '@') s
	mk
		| name == "." = if null date
			then use inhere
			else use . inuuid =<< getUUID
		| otherwise = use . inuuid =<< Remote.nameToUUID name
	use a = return $ Right $ \notpresent -> checkKey (a notpresent)
	inuuid u notpresent key
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
limitPresent :: Maybe UUID -> MatchFiles Annex
limitPresent u _ = checkKey $ \key -> do
	hereu <- getUUID
	if u == Just hereu || isNothing u
		then inAnnex key
		else do
			us <- Remote.keyLocations key
			return $ maybe False (`elem` us) u

{- Limit to content that is in a directory, anywhere in the repository tree -}
limitInDir :: FilePath -> MatchFiles Annex
limitInDir dir = const go
  where
	go (MatchingFile fi) = checkf $ matchFile fi
	go (MatchingKey _) = return False
	go (MatchingInfo af _ _ _) = checkf =<< getInfo af
	checkf = return . elem dir . splitPath . takeDirectory

{- Adds a limit to skip files not believed to have the specified number
 - of copies. -}
addCopies :: String -> Annex ()
addCopies = addLimit . limitCopies

limitCopies :: MkLimit Annex
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
			go' n good notpresent
	go' n good notpresent key = do
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

limitLackingCopies :: Bool -> MkLimit Annex
limitLackingCopies approx want = case readish want of
	Just needed -> Right $ \notpresent mi -> flip checkKey mi $
		go mi needed notpresent
	Nothing -> Left "bad value for number of lacking copies"
  where
	go mi needed notpresent key = do
		NumCopies numcopies <- if approx
			then approxNumCopies
			else case mi of
				MatchingFile fi -> getGlobalFileNumCopies $ matchFile fi
				MatchingKey _ -> approxNumCopies
				MatchingInfo _ _ _ _ -> approxNumCopies
		us <- filter (`S.notMember` notpresent)
			<$> (trustExclude UnTrusted =<< Remote.keyLocations key)
		return $ numcopies - length us >= needed
	approxNumCopies = fromMaybe defaultNumCopies <$> getGlobalNumCopies

{- Match keys that are unused.
 - 
 - This has a nice optimisation: When a file exists,
 - its key is obviously not unused.
 -}
limitUnused :: MatchFiles Annex
limitUnused _ (MatchingFile _) = return False
limitUnused _ (MatchingKey k) = S.member k <$> unusedKeys
limitUnused _ (MatchingInfo _ ak _ _) = do
	k <- getInfo ak
	S.member k <$> unusedKeys

{- Limit that matches any version of any file or key. -}
limitAnything :: MatchFiles Annex
limitAnything _ _ = return True

{- Limit that never matches. -}
limitNothing :: MatchFiles Annex
limitNothing _ _ = return False

{- Adds a limit to skip files not believed to be present in all
 - repositories in the specified group. -}
addInAllGroup :: String -> Annex ()
addInAllGroup groupname = addLimit $ limitInAllGroup groupMap groupname

limitInAllGroup :: Annex GroupMap -> MkLimit Annex
limitInAllGroup getgroupmap groupname = Right $ \notpresent mi -> do
	m <- getgroupmap
	let want = fromMaybe S.empty $ M.lookup groupname $ uuidsByGroup m
	if S.null want
		then return True
		-- optimisation: Check if a wanted uuid is notpresent.
		else if not (S.null (S.intersection want notpresent))
			then return False
			else checkKey (check want) mi
  where
	check want key = do
		present <- S.fromList <$> Remote.keyLocations key
		return $ S.null $ want `S.difference` present

{- Adds a limit to skip files not using a specified key-value backend. -}
addInBackend :: String -> Annex ()
addInBackend = addLimit . limitInBackend

limitInBackend :: MkLimit Annex
limitInBackend name = Right $ const $ checkKey check
  where
	check key = pure $ keyBackendName key == name

{- Adds a limit to skip files that are too large or too small -}
addLargerThan :: String -> Annex ()
addLargerThan = addLimit . limitSize (>)

addSmallerThan :: String -> Annex ()
addSmallerThan = addLimit . limitSize (<)

limitSize :: (Maybe Integer -> Maybe Integer -> Bool) -> MkLimit Annex
limitSize vs s = case readSize dataUnits s of
	Nothing -> Left "bad size"
	Just sz -> Right $ go sz
  where
	go sz _ (MatchingFile fi) = lookupFileKey fi >>= check fi sz
	go sz _ (MatchingKey key) = checkkey sz key
	go sz _ (MatchingInfo _ _ as _) =
		getInfo as >>= \sz' -> return (Just sz' `vs` Just sz)
	checkkey sz key = return $ keySize key `vs` Just sz
	check _ sz (Just key) = checkkey sz key
	check fi sz Nothing = do
		filesize <- liftIO $ catchMaybeIO $ getFileSize (currFile fi)
		return $ filesize `vs` Just sz

addMetaData :: String -> Annex ()
addMetaData = addLimit . limitMetaData

limitMetaData :: MkLimit Annex
limitMetaData s = case parseMetaDataMatcher s of
	Left e -> Left e
	Right (f, matching) -> Right $ const $ checkKey (check f matching)
  where
	check f matching k = not . S.null 
		. S.filter matching
		. metaDataValues f <$> getCurrentMetaData k

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
				shutdown True
				liftIO $ exitWith $ ExitFailure 101
			else return True

lookupFileKey :: FileInfo -> Annex (Maybe Key)
lookupFileKey = lookupFile . currFile

checkKey :: (Key -> Annex Bool) -> MatchInfo -> Annex Bool
checkKey a (MatchingFile fi) = lookupFileKey fi >>= maybe (return False) a
checkKey a (MatchingKey k) = a k
checkKey a (MatchingInfo _ ak _ _) = a =<< getInfo ak

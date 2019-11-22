{- user-specified limits on files to act on
 -
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Limit where

import Annex.Common
import qualified Annex
import qualified Utility.Matcher
import qualified Remote
import Annex.Content
import Annex.WorkTree
import Annex.Action
import Annex.UUID
import Annex.Magic
import Annex.Link
import Logs.Trust
import Annex.NumCopies
import Types.Key
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

import Data.Time.Clock.POSIX
import qualified Data.Set as S
import qualified Data.Map as M

{- Some limits can look at the current status of files on
 - disk, or in the annex. This allows controlling which happens. -}
data LimitBy = LimitDiskFiles | LimitAnnexFiles

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

{- Adds a new syntax token. -}
addSyntaxToken :: String -> Annex ()
addSyntaxToken = either error add . Utility.Matcher.syntaxToken

{- Adds a new limit. -}
addLimit :: Either String (MatchFiles Annex) -> Annex ()
addLimit = either giveup (\l -> add $ Utility.Matcher.Operation $ l S.empty)

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
	go (MatchingFile fi) = pure $ matchGlob cglob (matchFile fi)
	go (MatchingInfo p) = matchGlob cglob <$> getInfo (providedFilePath p)
	go (MatchingKey _ (AssociatedFile Nothing)) = pure False
	go (MatchingKey _ (AssociatedFile (Just af))) = pure $ matchGlob cglob af

addMimeType :: String -> Annex ()
addMimeType = addMagicLimit "mimetype" getMagicMimeType providedMimeType

addMimeEncoding :: String -> Annex ()
addMimeEncoding = addMagicLimit "mimeencoding" getMagicMimeEncoding providedMimeEncoding

addMagicLimit :: String -> (Magic -> FilePath -> Annex (Maybe String)) -> (ProvidedInfo -> OptInfo String) -> String -> Annex ()
addMagicLimit limitname querymagic selectprovidedinfo glob = do
	magic <- liftIO initMagicMime
	addLimit $ matchMagic limitname querymagic' selectprovidedinfo magic glob
  where
	querymagic' magic f = liftIO (isPointerFile f) >>= \case
		-- Avoid getting magic of a pointer file, which would
		-- wrongly be detected as text.
		Just _ -> return Nothing
		-- When the file is an annex symlink, get magic of the
		-- object file.
		Nothing -> isAnnexLink f >>= \case
			Just k -> withObjectLoc k $ querymagic magic
			Nothing -> querymagic magic f

matchMagic :: String -> (Magic -> FilePath -> Annex (Maybe String)) -> (ProvidedInfo -> OptInfo String) -> Maybe Magic -> MkLimit Annex
matchMagic _limitname querymagic selectprovidedinfo (Just magic) glob = Right $ const go
  where
 	cglob = compileGlob glob CaseSensative -- memoized
	go (MatchingKey _ _) = pure False
	go (MatchingFile fi) = catchBoolIO $
		maybe False (matchGlob cglob)
			<$> querymagic magic (currFile fi)
	go (MatchingInfo p) =
		matchGlob cglob <$> getInfo (selectprovidedinfo p)
matchMagic limitname _ _ Nothing _ = 
	Left $ "unable to load magic database; \""++limitname++"\" cannot be used"

addUnlocked :: Annex ()
addUnlocked = addLimit $ Right $ const $ matchLockStatus False

addLocked :: Annex ()
addLocked = addLimit $ Right $ const $ matchLockStatus True

matchLockStatus :: Bool -> MatchInfo -> Annex Bool
matchLockStatus _ (MatchingKey _ _) = pure False
matchLockStatus _ (MatchingInfo _) = pure False
matchLockStatus wantlocked (MatchingFile fi) = liftIO $ do
	islocked <- isPointerFile (currFile fi) >>= \case
		Just _key -> return False
		Nothing -> isSymbolicLink
			<$> getSymbolicLinkStatus (currFile fi)
	return (islocked == wantlocked)

{- Adds a limit to skip files not believed to be present
 - in a specfied repository. Optionally on a prior date. -}
addIn :: String -> Annex ()
addIn s = do
	u <- Remote.nameToUUID name
	hereu <- getUUID
	addLimit $ if u == hereu && null date
		then use inhere
		else use (inuuid u)
  where
	(name, date) = separate (== '@') s
	use a = Right $ checkKey . a
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
	go (MatchingKey _ (AssociatedFile Nothing)) = return False
	go (MatchingKey _ (AssociatedFile (Just af))) = checkf af
	go (MatchingInfo p) = checkf =<< getInfo (providedFilePath p)
	checkf = return . elem dir . splitPath . takeDirectory

{- Adds a limit to skip files not believed to have the specified number
 - of copies. -}
addCopies :: String -> Annex ()
addCopies = addLimit . limitCopies

limitCopies :: MkLimit Annex
limitCopies want = case splitc ':' want of
	-- Note that in case of a group having the same name as a trust
	-- level, it's parsed as a trust level, not as a group.
	[v, n] -> case parsetrustspec v of
		Just checker -> go n $ checktrust checker
		Nothing -> go n $ checkgroup (toGroup v)
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
		| "+" `isSuffixOf` s = (<=) <$> readTrustLevel (beginning s)
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
				MatchingKey _ _ -> approxNumCopies
				MatchingInfo {} -> approxNumCopies
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
limitUnused _ (MatchingKey k _) = S.member k <$> unusedKeys
limitUnused _ (MatchingInfo p) = do
	k <- getInfo (providedKey p)
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
	let want = fromMaybe S.empty $ M.lookup (toGroup groupname) $ uuidsByGroup m
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
	check key = pure $ fromKey keyVariety key == variety
	variety = parseKeyVariety (encodeBS name)

{- Adds a limit to skip files not using a secure hash. -}
addSecureHash :: Annex ()
addSecureHash = addLimit $ Right limitSecureHash

limitSecureHash :: MatchFiles Annex
limitSecureHash _ = checkKey $ pure . cryptographicallySecure . fromKey keyVariety

{- Adds a limit to skip files that are too large or too small -}
addLargerThan :: String -> Annex ()
addLargerThan = addLimit . limitSize LimitAnnexFiles (>)

addSmallerThan :: String -> Annex ()
addSmallerThan = addLimit . limitSize LimitAnnexFiles (<)

limitSize :: LimitBy -> (Maybe Integer -> Maybe Integer -> Bool) -> MkLimit Annex
limitSize lb vs s = case readSize dataUnits s of
	Nothing -> Left "bad size"
	Just sz -> Right $ go sz
  where
	go sz _ (MatchingFile fi) = case lb of
		LimitAnnexFiles -> lookupFileKey fi >>= \case
			Just key -> checkkey sz key
			Nothing -> return False
		LimitDiskFiles -> do
			filesize <- liftIO $ catchMaybeIO $ getFileSize (currFile fi)
			return $ filesize `vs` Just sz
	go sz _ (MatchingKey key _) = checkkey sz key
	go sz _ (MatchingInfo p) =
		getInfo (providedFileSize p) 
			>>= \sz' -> return (Just sz' `vs` Just sz)
	checkkey sz key = return $ fromKey keySize key `vs` Just sz

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

addTimeLimit :: Duration -> Annex ()
addTimeLimit duration = do
	start <- liftIO getPOSIXTime
	let cutoff = start + durationToPOSIXTime duration
	addLimit $ Right $ const $ const $ do
		now <- liftIO getPOSIXTime
		if now > cutoff
			then do
				warning $ "Time limit (" ++ fromDuration duration ++ ") reached!"
				shutdown True
				liftIO $ exitWith $ ExitFailure 101
			else return True

addAccessedWithin :: Duration -> Annex ()
addAccessedWithin duration = do
	now <- liftIO getPOSIXTime
	addLimit $ Right $ const $ checkKey $ check now
  where
	check now k = inAnnexCheck k $ \f ->
		liftIO $ catchDefaultIO False $ do
			s <- getFileStatus f
			let accessed = realToFrac (accessTime s)
			let delta = now - accessed
			return $ delta <= secs
	secs = fromIntegral (durationSeconds duration)

lookupFileKey :: FileInfo -> Annex (Maybe Key)
lookupFileKey = lookupFile . currFile

checkKey :: (Key -> Annex Bool) -> MatchInfo -> Annex Bool
checkKey a (MatchingFile fi) = lookupFileKey fi >>= maybe (return False) a
checkKey a (MatchingKey k _) = a k
checkKey a (MatchingInfo p) = a =<< getInfo (providedKey p)


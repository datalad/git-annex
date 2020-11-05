{- user-specified limits on files to act on
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
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
import qualified Utility.RawFilePath as R
import Backend

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
getMatcher = run <$> getMatcher'
  where
	run matcher i = Utility.Matcher.matchMrun matcher $ \o ->
		matchAction o S.empty i

getMatcher' :: Annex (Utility.Matcher.Matcher (MatchFiles Annex))
getMatcher' = go =<< Annex.getState Annex.limit
  where
	go (CompleteMatcher matcher) = return matcher
	go (BuildingMatcher l) = do
		let matcher = Utility.Matcher.generate (reverse l)
		Annex.changeState $ \s ->
			s { Annex.limit = CompleteMatcher matcher }
		return matcher

{- Checks if the user-specified limits contains anything that meets the
 - condition. -}
introspect :: (MatchFiles Annex -> Bool) -> Annex Bool
introspect c = Utility.Matcher.introspect c <$> getMatcher'

{- Adds something to the limit list, which is built up reversed. -}
add :: Utility.Matcher.Token (MatchFiles Annex) -> Annex ()
add l = Annex.changeState $ \s -> s { Annex.limit = prepend $ Annex.limit s }
  where
	prepend (BuildingMatcher ls) = BuildingMatcher (l:ls)
	prepend _ = error "internal"

{- Adds a new syntax token. -}
addSyntaxToken :: String -> Annex ()
addSyntaxToken = either error add . Utility.Matcher.syntaxToken

{- Adds a new limit. -}
addLimit :: Either String (MatchFiles Annex) -> Annex ()
addLimit = either giveup (add . Utility.Matcher.Operation)

{- Add a limit to skip files that do not match the glob. -}
addInclude :: String -> Annex ()
addInclude = addLimit . limitInclude

limitInclude :: MkLimit Annex
limitInclude glob = Right $ MatchFiles
	{ matchAction = const $ matchGlobFile glob
	, matchNeedsFileName = True
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	}

{- Add a limit to skip files that match the glob. -}
addExclude :: String -> Annex ()
addExclude = addLimit . limitExclude

limitExclude :: MkLimit Annex
limitExclude glob = Right $ MatchFiles
	{ matchAction = const $ not <$$> matchGlobFile glob
	, matchNeedsFileName = True
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	}

matchGlobFile :: String -> MatchInfo -> Annex Bool
matchGlobFile glob = go
  where
	cglob = compileGlob glob CaseSensative -- memoized
	go (MatchingFile fi) = pure $ matchGlob cglob (fromRawFilePath (matchFile fi))
	go (MatchingInfo p) = pure $ matchGlob cglob (fromRawFilePath (providedFilePath p))
	go (MatchingUserInfo p) = matchGlob cglob <$> getUserInfo (userProvidedFilePath p)
	go (MatchingKey _ (AssociatedFile Nothing)) = pure False
	go (MatchingKey _ (AssociatedFile (Just af))) = pure $ matchGlob cglob (fromRawFilePath af)

addMimeType :: String -> Annex ()
addMimeType = addMagicLimit "mimetype" getMagicMimeType providedMimeType userProvidedMimeType

addMimeEncoding :: String -> Annex ()
addMimeEncoding = addMagicLimit "mimeencoding" getMagicMimeEncoding providedMimeEncoding userProvidedMimeEncoding

addMagicLimit
	:: String
	-> (Magic -> FilePath -> Annex (Maybe String))
	-> (ProvidedInfo -> Maybe String)
	-> (UserProvidedInfo -> UserInfo String)
	-> String
	-> Annex ()
addMagicLimit limitname querymagic selectprovidedinfo selectuserprovidedinfo glob = do
	magic <- liftIO initMagicMime
	addLimit $ matchMagic limitname querymagic' selectprovidedinfo selectuserprovidedinfo magic glob
  where
	querymagic' magic f = liftIO (isPointerFile (toRawFilePath f)) >>= \case
		-- Avoid getting magic of a pointer file, which would
		-- wrongly be detected as text.
		Just _ -> return Nothing
		-- When the file is an annex symlink, get magic of the
		-- object file.
		Nothing -> isAnnexLink (toRawFilePath f) >>= \case
			Just k -> withObjectLoc k $
				querymagic magic . fromRawFilePath
			Nothing -> querymagic magic f

matchMagic
	:: String
	-> (Magic -> FilePath -> Annex (Maybe String))
	-> (ProvidedInfo -> Maybe String)
	-> (UserProvidedInfo -> UserInfo String)
	-> Maybe Magic
	-> MkLimit Annex
matchMagic _limitname querymagic selectprovidedinfo selectuserprovidedinfo (Just magic) glob = 
	Right $ MatchFiles
		{ matchAction = const go
		, matchNeedsFileName = True
		, matchNeedsFileContent = True
		, matchNeedsKey = False
		, matchNeedsLocationLog = False
		}
  where
 	cglob = compileGlob glob CaseSensative -- memoized
	go (MatchingKey _ _) = pure False
	go (MatchingFile fi) = case contentFile fi of
		Just f -> catchBoolIO $
			maybe False (matchGlob cglob)
				<$> querymagic magic (fromRawFilePath f)
		Nothing -> return False
	go (MatchingInfo p) = pure $
		maybe False (matchGlob cglob) (selectprovidedinfo p)
	go (MatchingUserInfo p) =
		matchGlob cglob <$> getUserInfo (selectuserprovidedinfo p)
matchMagic limitname _ _ _ Nothing _ = 
	Left $ "unable to load magic database; \""++limitname++"\" cannot be used"

addUnlocked :: Annex ()
addUnlocked = addLimit $ Right $ MatchFiles
	{ matchAction = const $ matchLockStatus False
	, matchNeedsFileName = True
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	}

addLocked :: Annex ()
addLocked = addLimit $ Right $ MatchFiles
	{ matchAction = const $ matchLockStatus True
	, matchNeedsFileName = True
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	}

matchLockStatus :: Bool -> MatchInfo -> Annex Bool
matchLockStatus _ (MatchingKey _ _) = pure False
matchLockStatus _ (MatchingInfo _) = pure False
matchLockStatus _ (MatchingUserInfo _) = pure False
matchLockStatus wantlocked (MatchingFile fi) = case contentFile fi of
	Just f -> liftIO $ do
		islocked <- isPointerFile f >>= \case
			Just _key -> return False
			Nothing -> isSymbolicLink
				<$> getSymbolicLinkStatus (fromRawFilePath f)
		return (islocked == wantlocked)
	Nothing -> return False

{- Adds a limit to skip files not believed to be present
 - in a specfied repository. Optionally on a prior date. -}
addIn :: String -> Annex ()
addIn s = do
	u <- Remote.nameToUUID name
	hereu <- getUUID
	addLimit $ if u == hereu && null date
		then use True checkinhere
		else use False (checkinuuid u)
  where
	(name, date) = separate (== '@') s
	use inhere a = Right $ MatchFiles
		{ matchAction = checkKey . a
		, matchNeedsFileName = False
		, matchNeedsFileContent = False
		, matchNeedsKey = True
		, matchNeedsLocationLog = not inhere
		}
	checkinuuid u notpresent key
		| null date = do
			us <- Remote.keyLocations key
			return $ u `elem` us && u `S.notMember` notpresent
		| otherwise = do
			us <- loggedLocationsHistorical (RefDate date) key
			return $ u `elem` us
	checkinhere notpresent key
		| S.null notpresent = inAnnex key
		| otherwise = do
			u <- getUUID
			if u `S.member` notpresent
				then return False
				else inAnnex key

{- Limit to content that is currently present on a uuid. -}
limitPresent :: Maybe UUID -> MatchFiles Annex
limitPresent u = MatchFiles
	{ matchAction = const $ checkKey $ \key -> do
		hereu <- getUUID
		if u == Just hereu || isNothing u
			then inAnnex key
			else do
				us <- Remote.keyLocations key
				return $ maybe False (`elem` us) u
	, matchNeedsFileName = False
	, matchNeedsFileContent = False
	, matchNeedsKey = True
	, matchNeedsLocationLog = not (isNothing u)
	}

{- Limit to content that is in a directory, anywhere in the repository tree -}
limitInDir :: FilePath -> MatchFiles Annex
limitInDir dir = MatchFiles 
	{ matchAction = const go
	, matchNeedsFileName = True
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	}
  where
	go (MatchingFile fi) = checkf $ fromRawFilePath $ matchFile fi
	go (MatchingKey _ (AssociatedFile Nothing)) = return False
	go (MatchingKey _ (AssociatedFile (Just af))) = checkf $ fromRawFilePath af
	go (MatchingInfo p) = checkf $ fromRawFilePath $ providedFilePath p
	go (MatchingUserInfo p) = checkf =<< getUserInfo (userProvidedFilePath p)
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
		Just n -> Right $ MatchFiles
			{ matchAction = \notpresent -> checkKey $
				go' n good notpresent
			, matchNeedsFileName = False
			, matchNeedsFileContent = False
			, matchNeedsKey = True
			, matchNeedsLocationLog = True
			}
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
	Just needed -> Right $ MatchFiles
		{ matchAction = \notpresent mi -> flip checkKey mi $
			go mi needed notpresent
		, matchNeedsFileName = False
		, matchNeedsFileContent = False
		, matchNeedsKey = True
		, matchNeedsLocationLog = True
		}
	Nothing -> Left "bad value for number of lacking copies"
  where
	go mi needed notpresent key = do
		NumCopies numcopies <- if approx
			then approxNumCopies
			else case mi of
				MatchingFile fi -> getGlobalFileNumCopies $
					matchFile fi
				MatchingKey _ _ -> approxNumCopies
				MatchingInfo {} -> approxNumCopies
				MatchingUserInfo {} -> approxNumCopies
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
limitUnused = MatchFiles
	{ matchAction = go
	, matchNeedsFileName = True
	, matchNeedsFileContent = False
	, matchNeedsKey = True
	, matchNeedsLocationLog = False
	}
  where
 	go _ (MatchingFile _) = return False
	go _ (MatchingKey k _) = isunused k
	go _ (MatchingInfo p) = maybe (pure False) isunused (providedKey p)
	go _ (MatchingUserInfo p) = do
		k <- getUserInfo (userProvidedKey p)
		isunused k
	
	isunused k = S.member k <$> unusedKeys

{- Limit that matches any version of any file or key. -}
limitAnything :: MatchFiles Annex
limitAnything = MatchFiles
	{ matchAction = \_ _ -> return True
	, matchNeedsFileName = False
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	}

{- Limit that never matches. -}
limitNothing :: MatchFiles Annex
limitNothing = MatchFiles 
	{ matchAction = \_ _ -> return False
	, matchNeedsFileName = False
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	}

{- Adds a limit to skip files not believed to be present in all
 - repositories in the specified group. -}
addInAllGroup :: String -> Annex ()
addInAllGroup groupname = addLimit $ limitInAllGroup groupMap groupname

limitInAllGroup :: Annex GroupMap -> MkLimit Annex
limitInAllGroup getgroupmap groupname = Right $ MatchFiles
	{ matchAction = \notpresent mi -> do
		m <- getgroupmap
		let want = fromMaybe S.empty $ M.lookup (toGroup groupname) $ uuidsByGroup m
		if S.null want
			then return True
			-- optimisation: Check if a wanted uuid is notpresent.
			else if not (S.null (S.intersection want notpresent))
				then return False
				else checkKey (check want) mi
	, matchNeedsFileName = False
	, matchNeedsFileContent = False
	, matchNeedsKey = True
	, matchNeedsLocationLog = True
	}
  where
	check want key = do
		present <- S.fromList <$> Remote.keyLocations key
		return $ S.null $ want `S.difference` present

{- Adds a limit to skip files not using a specified key-value backend. -}
addInBackend :: String -> Annex ()
addInBackend = addLimit . limitInBackend

limitInBackend :: MkLimit Annex
limitInBackend name = Right $ MatchFiles
	{ matchAction = const $ checkKey check
	, matchNeedsFileName = False
	, matchNeedsFileContent = False
	, matchNeedsKey = True
	, matchNeedsLocationLog = False
	}
  where
	check key = pure $ fromKey keyVariety key == variety
	variety = parseKeyVariety (encodeBS name)

{- Adds a limit to skip files not using a secure hash. -}
addSecureHash :: Annex ()
addSecureHash = addLimit $ Right limitSecureHash

limitSecureHash :: MatchFiles Annex
limitSecureHash = MatchFiles
	{ matchAction = const $ checkKey isCryptographicallySecure
	, matchNeedsFileName = False
	, matchNeedsFileContent = False
	, matchNeedsKey = True
	, matchNeedsLocationLog = False
	}

{- Adds a limit to skip files that are too large or too small -}
addLargerThan :: LimitBy -> String -> Annex ()
addLargerThan lb = addLimit . limitSize lb (>)

addSmallerThan :: LimitBy -> String -> Annex ()
addSmallerThan lb = addLimit . limitSize lb (<)

limitSize :: LimitBy -> (Maybe Integer -> Maybe Integer -> Bool) -> MkLimit Annex
limitSize lb vs s = case readSize dataUnits s of
	Nothing -> Left "bad size"
	Just sz -> Right $ MatchFiles
		{ matchAction = go sz
		, matchNeedsFileName = case lb of
			LimitAnnexFiles -> False
			LimitDiskFiles -> True
		, matchNeedsFileContent = False
		, matchNeedsKey = False
		, matchNeedsLocationLog = False
		}
  where
	go sz _ (MatchingFile fi) = case lb of
		LimitAnnexFiles -> goannexed sz fi
		LimitDiskFiles -> case contentFile fi of
			Just f -> do
				filesize <- liftIO $ catchMaybeIO $ getFileSize f
				return $ filesize `vs` Just sz
			Nothing -> goannexed sz fi
	go sz _ (MatchingKey key _) = checkkey sz key
	go sz _ (MatchingInfo p) = return $
		Just (providedFileSize p) `vs` Just sz
	go sz _ (MatchingUserInfo p) =
		getUserInfo (userProvidedFileSize p) 
			>>= \sz' -> return (Just sz' `vs` Just sz)
	goannexed sz fi = lookupFileKey fi >>= \case
		Just key -> checkkey sz key
		Nothing -> return False
	checkkey sz key = return $ fromKey keySize key `vs` Just sz

addMetaData :: String -> Annex ()
addMetaData = addLimit . limitMetaData

limitMetaData :: MkLimit Annex
limitMetaData s = case parseMetaDataMatcher s of
	Left e -> Left e
	Right (f, matching) -> Right $ MatchFiles
		{ matchAction = const $ checkKey (check f matching)
		, matchNeedsFileName = False
		, matchNeedsFileContent = False
		, matchNeedsKey = True
		, matchNeedsLocationLog = False
		}
  where
	check f matching k = not . S.null 
		. S.filter matching
		. metaDataValues f <$> getCurrentMetaData k

addTimeLimit :: Duration -> Annex ()
addTimeLimit duration = do
	start <- liftIO getPOSIXTime
	let cutoff = start + durationToPOSIXTime duration
	addLimit $ Right $ MatchFiles
		{ matchAction = const $ const $ do
			now <- liftIO getPOSIXTime
			if now > cutoff
				then do
					warning $ "Time limit (" ++ fromDuration duration ++ ") reached!"
					shutdown True
					liftIO $ exitWith $ ExitFailure 101
				else return True
		, matchNeedsFileName = False
		, matchNeedsFileContent = False
		, matchNeedsKey = False
		, matchNeedsLocationLog = False
		}

addAccessedWithin :: Duration -> Annex ()
addAccessedWithin duration = do
	now <- liftIO getPOSIXTime
	addLimit $ Right $ MatchFiles
		{ matchAction = const $ checkKey $ check now
		, matchNeedsFileName = False
		, matchNeedsFileContent = False
		, matchNeedsKey = False
		, matchNeedsLocationLog = False
		}
  where
	check now k = inAnnexCheck k $ \f ->
		liftIO $ catchDefaultIO False $ do
			s <- R.getFileStatus f
			let accessed = realToFrac (accessTime s)
			let delta = now - accessed
			return $ delta <= secs
	secs = fromIntegral (durationSeconds duration)

lookupFileKey :: FileInfo -> Annex (Maybe Key)
lookupFileKey fi = case contentFile fi of
	Just f -> lookupKey f
	Nothing -> return Nothing

checkKey :: (Key -> Annex Bool) -> MatchInfo -> Annex Bool
checkKey a (MatchingFile fi) = lookupFileKey fi >>= maybe (return False) a
checkKey a (MatchingKey k _) = a k
checkKey a (MatchingInfo p) = maybe (return False) a (providedKey p)
checkKey a (MatchingUserInfo p) = a =<< getUserInfo (userProvidedKey p)

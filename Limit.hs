{- user-specified limits on files to act on
 -
 - Copyright 2011-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Limit where

import Annex.Common
import qualified Annex
import qualified Utility.Matcher
import qualified Remote
import Annex.Content
import Annex.WorkTree
import Annex.UUID
import Annex.Magic
import Annex.RepoSize
import Annex.RepoSize.LiveUpdate
import Logs.MaxSize
import Annex.Link
import Types.Link
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
import Annex.CatFile
import Git.FilePath
import Git.Types (RefDate(..))
import Utility.Glob
import Utility.HumanTime
import Utility.DataUnits
import qualified Database.Keys
import qualified Utility.RawFilePath as R
import Backend

import Control.Monad.Writer
import Data.Time.Clock.POSIX
import qualified Data.Set as S
import qualified Data.Map as M
import qualified System.FilePath.ByteString as P
import System.PosixCompat.Files (accessTime, isSymbolicLink)

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
	run matcher i = do
		(match, desc) <- runWriterT $
			Utility.Matcher.matchMrun' matcher $ \o ->
				matchAction o NoLiveUpdate S.empty i
		explain (mkActionItem i) $ UnquotedString <$>
			Utility.Matcher.describeMatchResult matchDesc desc
				(if match then "matches:" else "does not match:")
		return match

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
addSyntaxToken = either giveup add . Utility.Matcher.syntaxToken

{- Adds a new limit. -}
addLimit :: Either String (MatchFiles Annex) -> Annex ()
addLimit = either giveup (add . Utility.Matcher.Operation)

{- Add a limit to skip files that do not match the glob. -}
addInclude :: String -> Annex ()
addInclude = addLimit . limitInclude

limitInclude :: MkLimit Annex
limitInclude glob = Right $ MatchFiles
	{ matchAction = const $ const $ matchGlobFile glob
	, matchNeedsFileName = True
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	, matchNeedsLiveRepoSize = False
	, matchDesc = "include" =? glob
	}

{- Add a limit to skip files that match the glob. -}
addExclude :: String -> Annex ()
addExclude = addLimit . limitExclude

limitExclude :: MkLimit Annex
limitExclude glob = Right $ MatchFiles
	{ matchAction = const $ const $ not <$$> matchGlobFile glob
	, matchNeedsFileName = True
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	, matchNeedsLiveRepoSize = False
	, matchDesc = "exclude" =? glob
	}

matchGlobFile :: String -> MatchInfo -> Annex Bool
matchGlobFile glob = go
  where
	cglob = compileGlob glob CaseSensitive (GlobFilePath True) -- memoized
	go (MatchingFile fi) = pure $ matchGlob cglob (fromRawFilePath (matchFile fi))
	go (MatchingInfo p) = pure $ case providedFilePath p of
		Just f -> matchGlob cglob (fromRawFilePath f)
		Nothing -> False
	go (MatchingUserInfo p) = matchGlob cglob <$> getUserInfo (userProvidedFilePath p)

{- Add a limit to skip files when there is no other file using the same
 - content, with a name matching the glob. -}
addIncludeSameContent :: String -> Annex ()
addIncludeSameContent = addLimit . limitIncludeSameContent

limitIncludeSameContent :: MkLimit Annex
limitIncludeSameContent glob = Right $ MatchFiles
	{ matchAction = const $ const $ matchSameContentGlob glob
	, matchNeedsFileName = True
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	, matchNeedsLiveRepoSize = False
	, matchDesc = "includesamecontent" =? glob
	}

{- Add a limit to skip files when there is no other file using the same
 - content, with a name matching the glob. -}
addExcludeSameContent :: String -> Annex ()
addExcludeSameContent = addLimit . limitExcludeSameContent

limitExcludeSameContent :: MkLimit Annex
limitExcludeSameContent glob = Right $ MatchFiles
	{ matchAction = const $ const $ not <$$> matchSameContentGlob glob
	, matchNeedsFileName = True
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	, matchNeedsLiveRepoSize = False
	, matchDesc = "excludesamecontent" =? glob
	}

matchSameContentGlob :: String -> MatchInfo -> Annex Bool
matchSameContentGlob glob mi = checkKey (go mi) mi
  where
	go (MatchingFile fi) k = check k (matchFile fi)
	go (MatchingInfo p) k = case providedFilePath p of
		Just f -> check k f
		Nothing -> return False
	go (MatchingUserInfo p) k = 
		check k . toRawFilePath
			=<< getUserInfo (userProvidedFilePath p)
	
	cglob = compileGlob glob CaseSensitive (GlobFilePath True) -- memoized
	
	matchesglob f = matchGlob cglob (fromRawFilePath f)
#ifdef mingw32_HOST_OS
		|| matchGlob cglob (fromRawFilePath (toInternalGitPath f))
#endif

	check k skipf = do
		-- Find other files with the same content, with filenames
		-- matching the glob.
		g <- Annex.gitRepo
		fs <- filter (/= P.normalise skipf)
			. filter matchesglob
			. map (\f -> P.normalise (fromTopFilePath f g))
			<$> Database.Keys.getAssociatedFiles k
		-- Some associated files in the keys database may no longer
		-- correspond to files in the repository. This is checked
		-- last as it's most expensive.
		anyM (\f -> maybe False (== k) <$> catKeyFile f) fs

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
matchMagic limitname querymagic selectprovidedinfo selectuserprovidedinfo (Just magic) glob = 
	Right $ MatchFiles
		{ matchAction = const $ const go
		, matchNeedsFileName = False
		, matchNeedsFileContent = True
		, matchNeedsKey = False
		, matchNeedsLocationLog = False
		, matchNeedsLiveRepoSize = False
		, matchDesc = limitname =? glob
		}
  where
 	cglob = compileGlob glob CaseSensitive (GlobFilePath False) -- memoized
	go (MatchingFile fi) = catchBoolIO $
		maybe False (matchGlob cglob)
			<$> querymagic magic (fromRawFilePath (contentFile fi))
	go (MatchingInfo p) = maybe
		(usecontent (providedKey p))
		(pure . matchGlob cglob)
		(selectprovidedinfo p)
	go (MatchingUserInfo p) =
		matchGlob cglob <$> getUserInfo (selectuserprovidedinfo p)
	usecontent (Just k) = withObjectLoc k $ \obj -> catchBoolIO $
		maybe False (matchGlob cglob)
			<$> querymagic magic (fromRawFilePath obj)
	usecontent Nothing = pure False
matchMagic limitname _ _ _ Nothing _ = 
	Left $ "unable to load magic database; \""++limitname++"\" cannot be used"

addUnlocked :: Annex ()
addUnlocked = addLimit $ Right $ MatchFiles
	{ matchAction = const $ const $ matchLockStatus False
	, matchNeedsFileName = True
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	, matchNeedsLiveRepoSize = False
	, matchDesc = matchDescSimple "unlocked"
	}

addLocked :: Annex ()
addLocked = addLimit $ Right $ MatchFiles
	{ matchAction = const $ const $ matchLockStatus True
	, matchNeedsFileName = True
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	, matchNeedsLiveRepoSize = False
	, matchDesc = matchDescSimple "locked"
	}

matchLockStatus :: Bool -> MatchInfo -> Annex Bool
matchLockStatus wantlocked (MatchingFile fi) = liftIO $ do
	let f = contentFile fi
	islocked <- isPointerFile f >>= \case
		Just _key -> return False
		Nothing -> isSymbolicLink
			<$> R.getSymbolicLinkStatus f
	return (islocked == wantlocked)
matchLockStatus wantlocked (MatchingInfo p) = 
	pure $ case providedLinkType p of
		Nothing -> False
		Just LockedLink -> wantlocked
		Just UnlockedLink -> not wantlocked
matchLockStatus _ (MatchingUserInfo _) = pure False

{- Adds a limit to skip files not believed to be present
 - in a specified repository. Optionally on a prior date. -}
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
		{ matchAction = const $ checkKey . a
		, matchNeedsFileName = False
		, matchNeedsFileContent = False
		, matchNeedsKey = True
		, matchNeedsLocationLog = not inhere
		, matchNeedsLiveRepoSize = False
		, matchDesc = "in" =? s
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

{- Limit to content that location tracking expects to be present
 - in the current repository. Does not verify inAnnex. -}
addExpectedPresent :: Annex ()
addExpectedPresent = do
	hereu <- getUUID
	addLimit $ Right $ MatchFiles
		{ matchAction = const $ const $ checkKey $ \key -> do
			us <- Remote.keyLocations key
			return $ hereu `elem` us
		, matchNeedsFileName = False
		, matchNeedsFileContent = False
		, matchNeedsKey = True
		, matchNeedsLocationLog = True
		, matchNeedsLiveRepoSize = False
		, matchDesc = matchDescSimple "expected-present"
		}

{- Limit to content that is currently present on a uuid. -}
limitPresent :: Maybe UUID -> MatchFiles Annex
limitPresent u = MatchFiles
	{ matchAction = const $ const $ checkKey $ \key -> do
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
	, matchNeedsLiveRepoSize = False
	, matchDesc = matchDescSimple "present"
	}

{- Limit to content that is in a directory, anywhere in the repository tree -}
limitInDir :: FilePath -> String -> MatchFiles Annex
limitInDir dir desc = MatchFiles 
	{ matchAction = const $ const go
	, matchNeedsFileName = True
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	, matchNeedsLiveRepoSize = False
	, matchDesc = matchDescSimple desc
	}
  where
	go (MatchingFile fi) = checkf $ fromRawFilePath $ matchFile fi
	go (MatchingInfo p) = maybe (pure False) (checkf . fromRawFilePath) (providedFilePath p)
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
			{ matchAction = const $ \notpresent -> checkKey $
				go' n good notpresent
			, matchNeedsFileName = False
			, matchNeedsFileContent = False
			, matchNeedsKey = True
			, matchNeedsLocationLog = True
			, matchNeedsLiveRepoSize = False
			, matchDesc = "copies" =? want
			}
	go' n good notpresent key = do
		us <- filter (`S.notMember` notpresent)
			<$> (filterM good =<< Remote.keyLocations key)
		return $ numCopiesCount us >= n
	checktrust checker u = checker <$> lookupTrust u
	checkgroup g u = S.member g <$> lookupGroups u
	parsetrustspec s
		| "+" `isSuffixOf` s = (<=) <$> readTrustLevel (beginning s)
		| otherwise = (==) <$> readTrustLevel s

{- Adds a limit to match files that need more copies made. -}
addLackingCopies :: String -> Bool -> String -> Annex ()
addLackingCopies desc approx = addLimit . limitLackingCopies desc approx

limitLackingCopies :: String -> Bool -> MkLimit Annex
limitLackingCopies desc approx want = case readish want of
	Just needed -> Right $ MatchFiles
		{ matchAction = const $ \notpresent mi -> flip checkKey mi $
			go mi needed notpresent
		, matchNeedsFileName = False
		, matchNeedsFileContent = False
		, matchNeedsKey = True
		, matchNeedsLocationLog = True
		, matchNeedsLiveRepoSize = False
		, matchDesc = matchDescSimple desc
		}
	Nothing -> Left "bad value for number of lacking copies"
  where
	go mi needed notpresent key = do
		numcopies <- if approx
			then approxNumCopies
			else case mi of
				MatchingFile fi -> getGlobalFileNumCopies $
					matchFile fi
				MatchingInfo {} -> approxNumCopies
				MatchingUserInfo {} -> approxNumCopies
		us <- filter (`S.notMember` notpresent)
			<$> (trustExclude UnTrusted =<< Remote.keyLocations key)
		let vs nhave numcopies' = numcopies' - nhave >= needed
		return $ numCopiesCheck'' us vs numcopies
	approxNumCopies = fromMaybe defaultNumCopies <$> getGlobalNumCopies

{- Match keys that are unused.
 - 
 - This has a nice optimisation: When a file exists,
 - its key is obviously not unused.
 -}
limitUnused :: MatchFiles Annex
limitUnused = MatchFiles
	{ matchAction = const $ const go
	, matchNeedsFileName = True
	, matchNeedsFileContent = False
	, matchNeedsKey = True
	, matchNeedsLocationLog = False
	, matchNeedsLiveRepoSize = False
	, matchDesc = matchDescSimple "unused"
	}
  where
 	go (MatchingFile _) = return False
	go (MatchingInfo p) = maybe (pure False) isunused (providedKey p)
	go (MatchingUserInfo p) = do
		k <- getUserInfo (userProvidedKey p)
		isunused k
	
	isunused k = S.member k <$> unusedKeys

{- Adds a limit that matches anything. -}
addAnything :: Annex ()
addAnything = addLimit (Right limitAnything)

{- Limit that matches any version of any file or key. -}
limitAnything :: MatchFiles Annex
limitAnything = MatchFiles
	{ matchAction = \_ _ _ -> return True
	, matchNeedsFileName = False
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	, matchNeedsLiveRepoSize = False
	, matchDesc = matchDescSimple "anything"
	}

{- Adds a limit that never matches. -}
addNothing :: Annex ()
addNothing = addLimit (Right limitNothing)

{- Limit that never matches. -}
limitNothing :: MatchFiles Annex
limitNothing = MatchFiles 
	{ matchAction = \_ _ _ -> return False
	, matchNeedsFileName = False
	, matchNeedsFileContent = False
	, matchNeedsKey = False
	, matchNeedsLocationLog = False
	, matchNeedsLiveRepoSize = False
	, matchDesc = matchDescSimple "nothing"
	}

{- Adds a limit to skip files not believed to be present in all
 - repositories in the specified group. -}
addInAllGroup :: String -> Annex ()
addInAllGroup groupname = addLimit $ limitInAllGroup groupMap groupname

limitInAllGroup :: Annex GroupMap -> MkLimit Annex
limitInAllGroup getgroupmap groupname = Right $ MatchFiles
	{ matchAction = const $ \notpresent mi -> do
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
	, matchNeedsLiveRepoSize = False
	, matchDesc = "inallgroup" =? groupname
	}
  where
	check want key = do
		present <- S.fromList <$> Remote.keyLocations key
		return $ S.null $ want `S.difference` present

{- Skip files unless they are present in at least one repository that is in
 - the specified group, and are not present in any repositories that are not
 - in the specified group. -}
addOnlyInGroup :: String -> Annex ()
addOnlyInGroup groupname = addLimit $ limitOnlyInGroup groupMap groupname

limitOnlyInGroup :: Annex GroupMap -> MkLimit Annex
limitOnlyInGroup getgroupmap groupname = Right $ MatchFiles
	{ matchAction = const $ \notpresent mi -> do
		m <- getgroupmap
		let want = fromMaybe S.empty $ M.lookup (toGroup groupname) $ uuidsByGroup m
		if S.null want
			then return False
			else checkKey (check notpresent want) mi
	, matchNeedsFileName = False
	, matchNeedsFileContent = False
	, matchNeedsKey = True
	, matchNeedsLocationLog = True
	, matchNeedsLiveRepoSize = False
	, matchDesc = "onlyingroup" =? groupname
	}
  where
	check notpresent want key = do
		locs <- S.fromList <$> Remote.keyLocations key
		let present = locs `S.difference` notpresent
		return $ not (S.null $ present `S.intersection` want)
			&& S.null (S.filter (`S.notMember` want) present)

limitBalanced :: Maybe UUID -> Annex GroupMap -> MkLimit Annex
limitBalanced mu getgroupmap groupname = do
	fullybalanced <- limitFullyBalanced' "balanced" mu getgroupmap groupname
	limitBalanced' "balanced" fullybalanced mu groupname 

limitBalanced' :: String -> MatchFiles Annex -> Maybe UUID -> MkLimit Annex
limitBalanced' termname fullybalanced mu groupname = do
	copies <- limitCopies $ if ':' `elem` groupname
		then groupname
		else groupname ++ ":1"
	let present = limitPresent mu
	Right $ MatchFiles
		{ matchAction = \lu a i ->
			ifM (Annex.getRead Annex.rebalance)
				( matchAction fullybalanced lu a i
				, matchAction present lu a i <||>
					((not <$> matchAction copies lu a i)
						<&&> matchAction fullybalanced lu a i
					)
				)
		, matchNeedsFileName =
			matchNeedsFileName present ||
			matchNeedsFileName fullybalanced ||
			matchNeedsFileName copies
		, matchNeedsFileContent =
			matchNeedsFileContent present ||
			matchNeedsFileContent fullybalanced ||
			matchNeedsFileContent copies
		, matchNeedsKey =
			matchNeedsKey present ||
			matchNeedsKey fullybalanced ||
			matchNeedsKey copies
		, matchNeedsLocationLog =
			matchNeedsLocationLog present ||
			matchNeedsLocationLog fullybalanced ||
			matchNeedsLocationLog copies
		, matchNeedsLiveRepoSize = True
		, matchDesc = termname =? groupname
		}

limitFullyBalanced :: Maybe UUID -> Annex GroupMap -> MkLimit Annex
limitFullyBalanced = limitFullyBalanced' "fullybalanced"

limitFullyBalanced' :: String -> Maybe UUID -> Annex GroupMap -> MkLimit Annex
limitFullyBalanced' = limitFullyBalanced'' $ \n key candidates -> do
	maxsizes <- getMaxSizes
	sizemap <- getLiveRepoSizes False
	threshhold <- annexFullyBalancedThreshhold <$> Annex.getGitConfig
	let toofull u =
		case (M.lookup u maxsizes, M.lookup u sizemap) of
			(Just (MaxSize maxsize), Just (RepoSize reposize)) ->
				fromIntegral reposize >= fromIntegral maxsize * threshhold
			_ -> False
	needsizebalance <- ifM (Annex.getRead Annex.rebalance)
		( return $ n > 1 &&
			n > S.size candidates 
				- S.size (S.filter toofull candidates)
		, return False
		)
	if needsizebalance
		then filterCandidatesFullySizeBalanced maxsizes sizemap n key candidates
		else do
			currentlocs <- S.fromList <$> loggedLocations key
 	 		let keysize = fromMaybe 0 (fromKey keySize key)
			let hasspace u = case (M.lookup u maxsizes, M.lookup u sizemap) of
				(Just maxsize, Just reposize) ->
					repoHasSpace keysize (u `S.member` currentlocs) reposize maxsize
				_ -> True
			return $ S.filter hasspace candidates

repoHasSpace :: Integer -> Bool -> RepoSize -> MaxSize -> Bool
repoHasSpace keysize inrepo (RepoSize reposize) (MaxSize maxsize)
	| inrepo =
		reposize <= maxsize
	| otherwise = 
		reposize + keysize <= maxsize

limitFullyBalanced''
	:: (Int -> Key -> S.Set UUID -> Annex (S.Set UUID))
	-> String
	-> Maybe UUID
	-> Annex GroupMap
	-> MkLimit Annex
limitFullyBalanced'' filtercandidates termname mu getgroupmap want =
	case splitc ':' want of
		[g] -> go g 1
		[g, n] -> maybe
			(Left $ "bad number for " ++ termname)
			(go g)
			(readish n)
		_ -> Left $ "bad value for " ++ termname
  where
	go s n = limitFullyBalanced''' filtercandidates termname mu
		getgroupmap (toGroup s) n want

limitFullyBalanced'''
	:: (Int -> Key -> S.Set UUID -> Annex (S.Set UUID))
	-> String
	-> Maybe UUID
	-> Annex GroupMap
	-> Group
	-> Int
	-> MkLimit Annex
limitFullyBalanced''' filtercandidates termname mu getgroupmap g n want = Right $ MatchFiles
	{ matchAction = \lu -> const $ checkKey $ \key -> do
		gm <- getgroupmap
		let groupmembers = fromMaybe S.empty $
			M.lookup g (uuidsByGroup gm)
		candidates <- filtercandidates n key groupmembers
		let wanted = if S.null candidates
			then False
			else case (mu, M.lookup g (balancedPickerByGroup gm)) of
				(Just u, Just picker) ->
					u `elem` picker candidates key n
				_ -> False
		when wanted $
			needLiveUpdate lu
		return wanted
	, matchNeedsFileName = False
	, matchNeedsFileContent = False
	, matchNeedsKey = True
	, matchNeedsLocationLog = False
	, matchNeedsLiveRepoSize = True
	, matchDesc = termname =? want
	}

limitSizeBalanced :: Maybe UUID -> Annex GroupMap -> MkLimit Annex
limitSizeBalanced mu getgroupmap groupname = do
	fullysizebalanced <- limitFullySizeBalanced' "sizebalanced" mu getgroupmap groupname
	limitBalanced' "sizebalanced" fullysizebalanced mu groupname 

limitFullySizeBalanced :: Maybe UUID -> Annex GroupMap -> MkLimit Annex
limitFullySizeBalanced = limitFullySizeBalanced' "fullysizebalanced"

limitFullySizeBalanced' :: String -> Maybe UUID -> Annex GroupMap -> MkLimit Annex
limitFullySizeBalanced' = limitFullyBalanced'' $ \n key candidates -> do
	maxsizes <- getMaxSizes
	sizemap <- getLiveRepoSizes False
	filterCandidatesFullySizeBalanced maxsizes sizemap n key candidates

filterCandidatesFullySizeBalanced
	:: M.Map UUID MaxSize
	-> M.Map UUID RepoSize
	-> Int
	-> Key
	-> S.Set UUID
	-> Annex (S.Set UUID)
filterCandidatesFullySizeBalanced maxsizes sizemap n key candidates = do
	currentlocs <- S.fromList <$> loggedLocations key
 	let keysize = fromMaybe 0 (fromKey keySize key)
	let go u = case (M.lookup u maxsizes, M.lookup u sizemap, u `S.member` currentlocs) of
		(Just maxsize, Just reposize, inrepo)
			| repoHasSpace keysize inrepo reposize maxsize ->
				proportionfree keysize inrepo u reposize maxsize
			| otherwise -> Nothing
		_ -> Nothing
	return $ S.fromList $
		map fst $ take n $ reverse $ sortOn snd $
		mapMaybe go $ S.toList candidates
  where
	proportionfree keysize inrepo u (RepoSize reposize) (MaxSize maxsize)
		| maxsize > 0 = Just
			( u
			, fromIntegral freespacesanskey / fromIntegral maxsize
				:: Double
			)
		| otherwise = Nothing
	  where
		freespacesanskey = maxsize - reposize +
			if inrepo then keysize else 0

{- Adds a limit to skip files not using a specified key-value backend. -}
addInBackend :: String -> Annex ()
addInBackend = addLimit . limitInBackend

limitInBackend :: MkLimit Annex
limitInBackend name = Right $ MatchFiles
	{ matchAction = const $ const $ checkKey check
	, matchNeedsFileName = False
	, matchNeedsFileContent = False
	, matchNeedsKey = True
	, matchNeedsLocationLog = False
	, matchNeedsLiveRepoSize = False
	, matchDesc = "inbackend" =? name
	}
  where
	check key = pure $ fromKey keyVariety key == variety
	variety = parseKeyVariety (encodeBS name)

{- Adds a limit to skip files not using a secure hash. -}
addSecureHash :: Annex ()
addSecureHash = addLimit $ Right limitSecureHash

limitSecureHash :: MatchFiles Annex
limitSecureHash = MatchFiles
	{ matchAction = const $ const $ checkKey isCryptographicallySecureKey
	, matchNeedsFileName = False
	, matchNeedsFileContent = False
	, matchNeedsKey = True
	, matchNeedsLocationLog = False
	, matchNeedsLiveRepoSize = False
	, matchDesc = matchDescSimple "securehash"
	}

{- Adds a limit to skip files that are too large or too small -}
addLargerThan :: LimitBy -> String -> Annex ()
addLargerThan lb = addLimit . limitSize lb "smallerthan" (>)

addSmallerThan :: LimitBy -> String -> Annex ()
addSmallerThan lb = addLimit . limitSize lb "smallerthan" (<)

limitSize :: LimitBy -> String -> (Maybe Integer -> Maybe Integer -> Bool) -> MkLimit Annex
limitSize lb desc vs s = case readSize dataUnits s of
	Nothing -> Left "bad size"
	Just sz -> Right $ MatchFiles
		{ matchAction = const $ go sz
		, matchNeedsFileName = case lb of
			LimitAnnexFiles -> False
			LimitDiskFiles -> True
		, matchNeedsFileContent = False
		, matchNeedsKey = False
		, matchNeedsLocationLog = False
		, matchNeedsLiveRepoSize = False
		, matchDesc = desc =? s
		}
  where
	go sz _ (MatchingFile fi) = case lb of
		LimitAnnexFiles -> lookupFileKey fi >>= \case
			Just key -> checkkey sz key
			Nothing -> return False
		LimitDiskFiles -> do
			filesize <- liftIO $ catchMaybeIO $ getFileSize (contentFile fi)
			return $ filesize `vs` Just sz
	go sz _ (MatchingInfo p) = case providedFileSize p of
		Just sz' -> pure (Just sz' `vs` Just sz)
		Nothing -> maybe (pure False) (checkkey sz) (providedKey p)
	go sz _ (MatchingUserInfo p) =
		getUserInfo (userProvidedFileSize p) 
			>>= \sz' -> return (Just sz' `vs` Just sz)
	checkkey sz key = return $ fromKey keySize key `vs` Just sz

addMetaData :: String -> Annex ()
addMetaData = addLimit . limitMetaData

limitMetaData :: MkLimit Annex
limitMetaData s = case parseMetaDataMatcher s of
	Left e -> Left e
	Right (f, matching) -> Right $ MatchFiles
		{ matchAction = const $ const $ checkKey (check f matching)
		, matchNeedsFileName = False
		, matchNeedsFileContent = False
		, matchNeedsKey = True
		, matchNeedsLocationLog = False
		, matchNeedsLiveRepoSize = False
		, matchDesc = "metadata" =? s
		}
  where
	check f matching k = not . S.null 
		. S.filter matching
		. metaDataValues f <$> getCurrentMetaData k

addAccessedWithin :: Duration -> Annex ()
addAccessedWithin duration = do
	now <- liftIO getPOSIXTime
	addLimit $ Right $ MatchFiles
		{ matchAction = const $ const $ checkKey $ check now
		, matchNeedsFileName = False
		, matchNeedsFileContent = False
		, matchNeedsKey = False
		, matchNeedsLocationLog = False
		, matchNeedsLiveRepoSize = False
		, matchDesc = "accessedwithin" =? fromDuration duration
		}
  where
	check now k = inAnnexCheck k $ \f ->
		liftIO $ catchDefaultIO False $ do
			s <- R.getSymbolicLinkStatus f
			let accessed = realToFrac (accessTime s)
			let delta = now - accessed
			return $ delta <= secs
	secs = fromIntegral (durationSeconds duration)

lookupFileKey :: FileInfo -> Annex (Maybe Key)
lookupFileKey fi = case matchKey fi of
	Just k -> return (Just k)
	Nothing -> lookupKey (contentFile fi)

checkKey :: (Key -> Annex Bool) -> MatchInfo -> Annex Bool
checkKey a (MatchingFile fi) = lookupFileKey fi >>= maybe (return False) a
checkKey a (MatchingInfo p) = maybe (return False) a (providedKey p)
checkKey a (MatchingUserInfo p) = a =<< getUserInfo (userProvidedKey p)

matchDescSimple :: String -> (Bool -> Utility.Matcher.MatchDesc)
matchDescSimple s b = Utility.Matcher.MatchDesc $ s ++
	if b then "[TRUE]" else "[FALSE]"

(=?) :: String -> String -> (Bool -> Utility.Matcher.MatchDesc)
k =? v = matchDescSimple (k ++ "=" ++ v)

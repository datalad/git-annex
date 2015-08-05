{- git-annex remotes
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote (
	Remote,
	uuid,
	name,
	storeKey,
	retrieveKeyFile,
	retrieveKeyFileCheap,
	removeKey,
	hasKey,
	hasKeyCheap,
	whereisKey,
	remoteFsck,

	remoteTypes,
	remoteList,
	remoteList',
	gitSyncableRemote,
	remoteMap,
	remoteMap',
	uuidDescriptions,
	byName,
	byName',
	byNameOrGroup,
	byNameOnly,
	byNameWithUUID,
	byCost,
	prettyPrintUUIDs,
	prettyPrintUUIDsWith,
	prettyListUUIDs,
	prettyUUID,
	remoteFromUUID,
	remotesWithUUID,
	remotesWithoutUUID,
	keyLocations,
	keyPossibilities,
	keyPossibilitiesTrusted,
	nameToUUID,
	nameToUUID',
	showTriedRemotes,
	showLocations,
	forceTrust,
	logStatus,
	checkAvailable,
	isXMPPRemote,
	claimingUrl,
) where

import qualified Data.Map as M
import Text.JSON
import Text.JSON.Generic
import Data.Ord

import Common.Annex
import Types.Remote
import qualified Annex
import Annex.UUID
import Logs.UUID
import Logs.Trust
import Logs.Location hiding (logStatus)
import Logs.Web
import Remote.List
import Config
import Git.Types (RemoteName)
import qualified Git

{- Map from UUIDs of Remotes to a calculated value. -}
remoteMap :: (Remote -> v) -> Annex (M.Map UUID v)
remoteMap mkv = remoteMap' mkv mkk
  where
	mkk r = case uuid r of
		NoUUID -> Nothing
		u -> Just u

remoteMap' :: Ord k => (Remote -> v) -> (Remote -> Maybe k) -> Annex (M.Map k v)
remoteMap' mkv mkk = M.fromList . mapMaybe mk <$> remoteList
  where
	mk r = case mkk r of
		Nothing -> Nothing
		Just k -> Just (k, mkv r)

{- Map of UUIDs of repositories and their descriptions.
 - The names of Remotes are added to suppliment any description that has
 - been set for a repository.  -}
uuidDescriptions :: Annex (M.Map UUID String)
uuidDescriptions = M.unionWith addName <$> uuidMap <*> remoteMap name

addName :: String -> RemoteName -> String
addName desc n
	| desc == n || null desc = "[" ++ n ++ "]"
	| otherwise = desc ++ " [" ++ n ++ "]"

{- When a name is specified, looks up the remote matching that name.
 - (Or it can be a UUID.)
 -
 - Throws an error if a name is specified and no matching remote can be
 - found.
 -}
byName :: Maybe RemoteName -> Annex (Maybe Remote)
byName Nothing = return Nothing
byName (Just n) = either error Just <$> byName' n

{- Like byName, but the remote must have a configured UUID. -}
byNameWithUUID :: Maybe RemoteName -> Annex (Maybe Remote)
byNameWithUUID = checkuuid <=< byName
  where
	checkuuid Nothing = return Nothing
	checkuuid (Just r)
		| uuid r == NoUUID = error $
			if remoteAnnexIgnore (gitconfig r)
				then noRemoteUUIDMsg r ++
					" (" ++ show (remoteConfig (repo r) "ignore") ++
					" is set)"
				else noRemoteUUIDMsg r
		| otherwise = return $ Just r

byName' :: RemoteName -> Annex (Either String Remote)
byName' "" = return $ Left "no remote specified"
byName' n = go . filter matching <$> remoteList
  where
	go [] = Left $ "there is no available git remote named \"" ++ n ++ "\""
	go (match:_) = Right match
	matching r = n == name r || toUUID n == uuid r

{- Finds the remote or remote group matching the name. -}
byNameOrGroup :: RemoteName -> Annex [Remote]
byNameOrGroup n = go =<< getConfigMaybe (ConfigKey ("remotes." ++ n))
  where
	go (Just l) = catMaybes <$> mapM (byName . Just) (split " " l)
	go Nothing = maybeToList <$> byName (Just n)

{- Only matches remote name, not UUID -}
byNameOnly :: RemoteName -> Annex (Maybe Remote)
byNameOnly n = headMaybe . filter matching <$> remoteList
  where
	matching r = n == name r

noRemoteUUIDMsg :: Remote -> String
noRemoteUUIDMsg r = "cannot determine uuid for " ++ name r

{- Looks up a remote by name (or by UUID, or even by description),
 - and returns its UUID. Finds even repositories that are not
 - configured in .git/config. -}
nameToUUID :: RemoteName -> Annex UUID
nameToUUID = either error return <=< nameToUUID'

nameToUUID' :: RemoteName -> Annex (Either String UUID)
nameToUUID' "." = Right <$> getUUID -- special case for current repo
nameToUUID' "here" = Right <$> getUUID
nameToUUID' n = byName' n >>= go
  where
	go (Right r) = return $ case uuid r of
		NoUUID -> Left $ noRemoteUUIDMsg r
		u -> Right u
	go (Left e) = do
		m <- uuidMap
		return $ case M.keys (M.filter (== n) m) of
			[u] -> Right u
			[] -> let u = toUUID n
				in case M.keys (M.filterWithKey (\k _ -> k == u) m) of
					[] -> Left e
					_ -> Right u
			_us -> Left "Found multiple repositories with that description"

{- Pretty-prints a list of UUIDs of remotes, for human display.
 -
 - When JSON is enabled, also outputs a machine-readable description
 - of the UUIDs. -}
prettyPrintUUIDs :: String -> [UUID] -> Annex String
prettyPrintUUIDs desc uuids = prettyPrintUUIDsWith Nothing desc $
	zip uuids (repeat (Nothing :: Maybe String))

{- An optional field can be included in the list of UUIDs. -}
prettyPrintUUIDsWith
	:: (JSON v, Show v) 
	=> Maybe String 
	-> String 
	-> [(UUID, Maybe v)] 
	-> Annex String
prettyPrintUUIDsWith optfield desc uuids = do
	hereu <- getUUID
	m <- uuidDescriptions
	maybeShowJSON [(desc, map (jsonify m hereu) uuids)]
	return $ unwords $ map (\u -> "\t" ++ prettify m hereu u ++ "\n") uuids
  where
	finddescription m u = M.findWithDefault "" u m
	prettify m hereu (u, optval)
		| not (null d) = addoptval $ fromUUID u ++ " -- " ++ d
		| otherwise = addoptval $ fromUUID u
	  where
		ishere = hereu == u
		n = finddescription m u
		d
			| null n && ishere = "here"
			| ishere = addName n "here"
			| otherwise = n
		addoptval s = case optval of
			Nothing -> s
			Just val -> show val ++ ": " ++ s
	jsonify m hereu (u, optval) = toJSObject $ catMaybes
		[ Just ("uuid", toJSON $ fromUUID u)
		, Just ("description", toJSON $ finddescription m u)
		, Just ("here", toJSON $ hereu == u)
		, case (optfield, optval) of
			(Just field, Just val) -> Just (field, showJSON val)
			_ -> Nothing
		]

{- List of remote names and/or descriptions, for human display.  -}
prettyListUUIDs :: [UUID] -> Annex [String]
prettyListUUIDs uuids = do
	hereu <- getUUID
	m <- uuidDescriptions
	return $ map (prettify m hereu) uuids
  where
	finddescription m u = M.findWithDefault "" u m
	prettify m hereu u
		| u == hereu = addName n "here"
		| otherwise = n
	  where
		n = finddescription m u

{- Nice display of a remote's name and/or description. -}
prettyUUID :: UUID -> Annex String
prettyUUID u = concat <$> prettyListUUIDs [u]

{- Gets the remote associated with a UUID. -}
remoteFromUUID :: UUID -> Annex (Maybe Remote)
remoteFromUUID u = ifM ((==) u <$> getUUID)
	( return Nothing
	, maybe tryharder (return . Just) =<< findinmap
	)
  where
	findinmap = M.lookup u <$> remoteMap id
	{- Re-read remote list in case a new remote has popped up. -}
	tryharder = do
		void remoteListRefresh
		findinmap

{- Filters a list of remotes to ones that have the listed uuids. -}
remotesWithUUID :: [Remote] -> [UUID] -> [Remote]
remotesWithUUID rs us = filter (\r -> uuid r `elem` us) rs

{- Filters a list of remotes to ones that do not have the listed uuids. -}
remotesWithoutUUID :: [Remote] -> [UUID] -> [Remote]
remotesWithoutUUID rs us = filter (\r -> uuid r `notElem` us) rs

{- List of repository UUIDs that the location log indicates may have a key.
 - Dead repositories are excluded. -}
keyLocations :: Key -> Annex [UUID]
keyLocations key = trustExclude DeadTrusted =<< loggedLocations key

{- Cost ordered lists of remotes that the location log indicates
 - may have a key.
 -}
keyPossibilities :: Key -> Annex [Remote]
keyPossibilities key = fst <$> keyPossibilities' key []

{- Cost ordered lists of remotes that the location log indicates
 - may have a key.
 -
 - Also returns a list of UUIDs that are trusted to have the key
 - (some may not have configured remotes).
 -}
keyPossibilitiesTrusted :: Key -> Annex ([Remote], [UUID])
keyPossibilitiesTrusted key = keyPossibilities' key =<< trustGet Trusted

keyPossibilities' :: Key -> [UUID] -> Annex ([Remote], [UUID])
keyPossibilities' key trusted = do
	u <- getUUID

	-- uuids of all remotes that are recorded to have the key
	validuuids <- filter (/= u) <$> keyLocations key

	-- note that validuuids is assumed to not have dups
	let validtrusteduuids = validuuids `intersect` trusted

	-- remotes that match uuids that have the key
	allremotes <- filter (not . remoteAnnexIgnore . gitconfig)
		<$> remoteList
	let validremotes = remotesWithUUID allremotes validuuids

	return (sortBy (comparing cost) validremotes, validtrusteduuids)

{- Displays known locations of a key. -}
showLocations :: Bool -> Key -> [UUID] -> String -> Annex ()
showLocations separateuntrusted key exclude nolocmsg = do
	u <- getUUID
	uuids <- keyLocations key
	untrusteduuids <- if separateuntrusted
		then trustGet UnTrusted
		else pure []
	let uuidswanted = filteruuids uuids (u:exclude++untrusteduuids) 
	let uuidsskipped = filteruuids uuids (u:exclude++uuidswanted)
	ppuuidswanted <- prettyPrintUUIDs "wanted" uuidswanted
	ppuuidsskipped <- prettyPrintUUIDs "skipped" uuidsskipped
	let msg = message ppuuidswanted ppuuidsskipped
	unless (null msg) $
		showLongNote msg
	ignored <- filter (remoteAnnexIgnore . gitconfig) <$> remoteList
	unless (null ignored) $
		showLongNote $ "(Note that these git remotes have annex-ignore set: " ++ unwords (map name ignored) ++ ")"
  where
	filteruuids l x = filter (`notElem` x) l
	message [] [] = nolocmsg
	message rs [] = "Try making some of these repositories available:\n" ++ rs
	message [] us = "Also these untrusted repositories may contain the file:\n" ++ us
	message rs us = message rs [] ++ message [] us

showTriedRemotes :: [Remote] -> Annex ()
showTriedRemotes [] = noop
showTriedRemotes remotes =
	showLongNote $ "Unable to access these remotes: " ++
		intercalate ", " (map name remotes)

forceTrust :: TrustLevel -> String -> Annex ()
forceTrust level remotename = do
	u <- nameToUUID remotename
	Annex.changeState $ \s ->
		s { Annex.forcetrust = M.insert u level (Annex.forcetrust s) }

{- Used to log a change in a remote's having a key. The change is logged
 - in the local repo, not on the remote. The process of transferring the
 - key to the remote, or removing the key from it *may* log the change
 - on the remote, but this cannot always be relied on. -}
logStatus :: Remote -> Key -> LogStatus -> Annex ()
logStatus remote key = logChange key (uuid remote)

{- Orders remotes by cost, with ones with the lowest cost grouped together. -}
byCost :: [Remote] -> [[Remote]]
byCost = map snd . sortBy (comparing fst) . M.toList . costmap
  where
	costmap = M.fromListWith (++) . map costpair
	costpair r = (cost r, [r])

checkAvailable :: Bool -> Remote -> IO Bool
checkAvailable assumenetworkavailable = 
	maybe (return assumenetworkavailable) doesDirectoryExist . localpath

{- Remotes using the XMPP transport have urls like xmpp::user@host -}
isXMPPRemote :: Remote -> Bool
isXMPPRemote remote = Git.repoIsUrl r && "xmpp::" `isPrefixOf` Git.repoLocation r
  where
	r = repo remote

hasKey :: Remote -> Key -> Annex (Either String Bool)
hasKey r k = either (Left  . show) Right <$> tryNonAsync (checkPresent r k)

hasKeyCheap :: Remote -> Bool
hasKeyCheap = checkPresentCheap

{- The web special remote claims urls by default. -}
claimingUrl :: URLString -> Annex Remote
claimingUrl url = do
	rs <- remoteList
	let web = Prelude.head $ filter (\r -> uuid r == webUUID) rs
	fromMaybe web <$> firstM checkclaim rs
  where
	checkclaim = maybe (pure False) (`id` url) . claimUrl

{- git-annex remotes
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
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

	remoteTypes,
	remoteList,
	enabledRemoteList,
	remoteMap,
	uuidDescriptions,
	byName,
	prettyPrintUUIDs,
	remotesWithUUID,
	remotesWithoutUUID,
	keyLocations,
	keyPossibilities,
	keyPossibilitiesTrusted,
	nameToUUID,
	showTriedRemotes,
	showLocations,
	forceTrust,
	logStatus
) where

import qualified Data.Map as M
import Text.JSON
import Text.JSON.Generic

import Common.Annex
import Types.Remote
import qualified Annex
import Annex.UUID
import Logs.UUID
import Logs.Trust
import Logs.Location
import Remote.List

{- Map from UUIDs of Remotes to a calculated value. -}
remoteMap :: (Remote -> a) -> Annex (M.Map UUID a)
remoteMap c = M.fromList . map (\r -> (uuid r, c r)) .
	filter (\r -> uuid r /= NoUUID) <$> remoteList

{- Map of UUIDs and their descriptions.
 - The names of Remotes are added to suppliment any description that has
 - been set for a repository. -}
uuidDescriptions :: Annex (M.Map UUID String)
uuidDescriptions = M.unionWith addName <$> uuidMap <*> remoteMap name

addName :: String -> String -> String
addName desc n
	| desc == n = desc
	| null desc = n
	| otherwise = n ++ " (" ++ desc ++ ")"

{- When a name is specified, looks up the remote matching that name.
 - Only finds currently configured git remotes. -}
byName :: Maybe String -> Annex (Maybe Remote)
byName Nothing = return Nothing
byName (Just n) = do
	res <- byName' n
	case res of
		Left e -> error e
		Right r -> return $ Just r
byName' :: String -> Annex (Either String Remote)
byName' "" = return $ Left "no remote specified"
byName' n = do
	match <- filter matching <$> remoteList
	if null match
		then return $ Left $ "there is no git remote named \"" ++ n ++ "\""
		else return $ Right $ Prelude.head match
	where
		matching r = n == name r || toUUID n == uuid r

{- Looks up a remote by name (or by UUID, or even by description),
 - and returns its UUID. Finds even remotes that are not configured in
 - .git/config. -}
nameToUUID :: String -> Annex UUID
nameToUUID "." = getUUID -- special case for current repo
nameToUUID "" = error "no remote specified"
nameToUUID n = byName' n >>= go
	where
		go (Right r) = return $ uuid r
		go (Left e) = fromMaybe (error e) <$> bydescription
		bydescription = do
			m <- uuidMap
			case M.lookup n $ transform swap m of
				Just u -> return $ Just u
				Nothing -> return $ byuuid m
		byuuid m = M.lookup (toUUID n) $ transform double m
		transform a = M.fromList . map a . M.toList
		swap (a, b) = (b, a)
		double (a, _) = (a, a)

{- Pretty-prints a list of UUIDs of remotes, for human display.
 -
 - Shows descriptions from the uuid log, falling back to remote names,
 - as some remotes may not be in the uuid log.
 -
 - When JSON is enabled, also generates a machine-readable description
 - of the UUIDs. -}
prettyPrintUUIDs :: String -> [UUID] -> Annex String
prettyPrintUUIDs desc uuids = do
	hereu <- getUUID
	m <- uuidDescriptions
	maybeShowJSON [(desc, map (jsonify m hereu) uuids)]
	return $ unwords $ map (\u -> "\t" ++ prettify m hereu u ++ "\n") uuids
	where
		finddescription m u = M.findWithDefault "" u m
		prettify m hereu u
			| not (null d) = fromUUID u ++ " -- " ++ d
			| otherwise = fromUUID u
			where
				ishere = hereu == u
				n = finddescription m u
				d
					| null n && ishere = "here"
					| ishere = addName n "here"
					| otherwise = n
		jsonify m hereu u = toJSObject
			[ ("uuid", toJSON $ fromUUID u)
			, ("description", toJSON $ finddescription m u)
			, ("here", toJSON $ hereu == u)
			]

{- Filters a list of remotes to ones that have the listed uuids. -}
remotesWithUUID :: [Remote] -> [UUID] -> [Remote]
remotesWithUUID rs us = filter (\r -> uuid r `elem` us) rs

{- Filters a list of remotes to ones that do not have the listed uuids. -}
remotesWithoutUUID :: [Remote] -> [UUID] -> [Remote]
remotesWithoutUUID rs us = filter (\r -> uuid r `notElem` us) rs

{- List of repository UUIDs that the location log indicates may have a key.
 - Dead repositories are excluded. -}
keyLocations :: Key -> Annex [UUID]
keyLocations key = snd <$> (trustPartition DeadTrusted =<< loggedLocations key)

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
	allremotes <- enabledRemoteList
	let validremotes = remotesWithUUID allremotes validuuids

	return (sort validremotes, validtrusteduuids)

{- Displays known locations of a key. -}
showLocations :: Key -> [UUID] -> Annex ()
showLocations key exclude = do
	u <- getUUID
	uuids <- keyLocations key
	untrusteduuids <- trustGet UnTrusted
	let uuidswanted = filteruuids uuids (u:exclude++untrusteduuids) 
	let uuidsskipped = filteruuids uuids (u:exclude++uuidswanted)
	ppuuidswanted <- Remote.prettyPrintUUIDs "wanted" uuidswanted
	ppuuidsskipped <- Remote.prettyPrintUUIDs "skipped" uuidsskipped
	showLongNote $ message ppuuidswanted ppuuidsskipped
	where
		filteruuids l x = filter (`notElem` x) l
		message [] [] = "No other repository is known to contain the file."
		message rs [] = "Try making some of these repositories available:\n" ++ rs
		message [] us = "Also these untrusted repositories may contain the file:\n" ++ us
		message rs us = message rs [] ++ message [] us

showTriedRemotes :: [Remote] -> Annex ()
showTriedRemotes [] = return ()	
showTriedRemotes remotes =
	showLongNote $ "Unable to access these remotes: " ++
		join ", " (map name remotes)

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
logStatus remote key present = logChange key (uuid remote) present

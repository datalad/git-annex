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
	removeKey,
	hasKey,
	hasKeyCheap,

	remoteTypes,
	byName,
	prettyPrintUUIDs,
	remotesWithUUID,
	remotesWithoutUUID,
	keyPossibilities,
	keyPossibilitiesTrusted,
	nameToUUID,
	showTriedRemotes,
	showLocations,
	forceTrust
) where

import qualified Data.Map as M
import Text.JSON
import Text.JSON.Generic

import Common.Annex
import Types.Remote
import UUID
import qualified Annex
import Config
import Trust
import LocationLog
import RemoteLog

import qualified Remote.Git
import qualified Remote.S3
import qualified Remote.Bup
import qualified Remote.Directory
import qualified Remote.Rsync
import qualified Remote.Web
import qualified Remote.Hook

remoteTypes :: [RemoteType Annex]
remoteTypes =
	[ Remote.Git.remote
	, Remote.S3.remote
	, Remote.Bup.remote
	, Remote.Directory.remote
	, Remote.Rsync.remote
	, Remote.Web.remote
	, Remote.Hook.remote
	]

{- Builds a list of all available Remotes.
 - Since doing so can be expensive, the list is cached. -}
genList :: Annex [Remote Annex]
genList = do
	rs <- Annex.getState Annex.remotes
	if null rs
		then do
			m <- readRemoteLog
			l <- mapM (process m) remoteTypes
			let rs' = concat l
			Annex.changeState $ \s -> s { Annex.remotes = rs' }
			return rs'
		else return rs
	where
		process m t = 
			enumerate t >>=
			mapM (gen m t)
		gen m t r = do
			u <- getUUID r
			generate t r u (M.lookup u m)

{- Looks up a remote by name. (Or by UUID.) Only finds currently configured
 - git remotes. -}
byName :: String -> Annex (Remote Annex)
byName n = do
	res <- byName' n
	case res of
		Left e -> error e
		Right r -> return r
byName' :: String -> Annex (Either String (Remote Annex))
byName' "" = return $ Left "no remote specified"
byName' n = do
	allremotes <- genList
	let match = filter matching allremotes
	if null match
		then return $ Left $ "there is no git remote named \"" ++ n ++ "\""
		else return $ Right $ head match
	where
		matching r = n == name r || n == uuid r

{- Looks up a remote by name (or by UUID, or even by description),
 - and returns its UUID. Finds even remotes that are not configured in
 - .git/config. -}
nameToUUID :: String -> Annex UUID
nameToUUID "." = getUUID =<< gitRepo -- special case for current repo
nameToUUID n = do
	res <- byName' n
	case res of
		Left e -> fromMaybe (error e) <$> byDescription
		Right r -> return $ uuid r
	where
		byDescription = do
			m <- uuidMap
			case M.lookup n $ transform swap m of
				Just u -> return $ Just u
				Nothing -> return $ M.lookup n $ transform double m
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
	here <- getUUID =<< gitRepo
	m <- M.unionWith addname <$> uuidMap <*> remoteMap
	maybeShowJSON [(desc, map (jsonify m here) uuids)]
	return $ unwords $ map (\u -> "\t" ++ prettify m here u ++ "\n") uuids
	where
		addname d n
			| d == n = d
			| otherwise = n ++ " (" ++ d ++ ")"
		remoteMap = M.fromList . map (\r -> (uuid r, name r)) <$> genList
		findlog m u = M.findWithDefault "" u m
		prettify m here u
			| not (null d) = u ++ " -- " ++ d
			| otherwise = u
			where
				ishere = here == u
				n = findlog m u
				d
					| null n && ishere = "here"
					| ishere = addname n "here"
					| otherwise = n
		jsonify m here u = toJSObject
			[ ("uuid", toJSON u)
			, ("description", toJSON $ findlog m u)
			, ("here", toJSON $ here == u)
			]

{- Filters a list of remotes to ones that have the listed uuids. -}
remotesWithUUID :: [Remote Annex] -> [UUID] -> [Remote Annex]
remotesWithUUID rs us = filter (\r -> uuid r `elem` us) rs

{- Filters a list of remotes to ones that do not have the listed uuids. -}
remotesWithoutUUID :: [Remote Annex] -> [UUID] -> [Remote Annex]
remotesWithoutUUID rs us = filter (\r -> uuid r `notElem` us) rs

{- Cost ordered lists of remotes that the LocationLog indicate may have a key.
 -}
keyPossibilities :: Key -> Annex [Remote Annex]
keyPossibilities key = fst <$> keyPossibilities' False key

{- Cost ordered lists of remotes that the LocationLog indicate may have a key.
 -
 - Also returns a list of UUIDs that are trusted to have the key
 - (some may not have configured remotes).
 -}
keyPossibilitiesTrusted :: Key -> Annex ([Remote Annex], [UUID])
keyPossibilitiesTrusted = keyPossibilities' True

keyPossibilities' :: Bool -> Key -> Annex ([Remote Annex], [UUID])
keyPossibilities' withtrusted key = do
	g <- gitRepo
	u <- getUUID g
	trusted <- if withtrusted then trustGet Trusted else return []

	-- get uuids of all remotes that are recorded to have the key
	uuids <- keyLocations key
	let validuuids = filter (/= u) uuids

	-- note that validuuids is assumed to not have dups
	let validtrusteduuids = validuuids `intersect` trusted

	-- remotes that match uuids that have the key
	allremotes <- filterM (repoNotIgnored . repo) =<< genList
	let validremotes = remotesWithUUID allremotes validuuids

	return (sort validremotes, validtrusteduuids)

{- Displays known locations of a key. -}
showLocations :: Key -> [UUID] -> Annex ()
showLocations key exclude = do
	g <- gitRepo
	u <- getUUID g
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

showTriedRemotes :: [Remote Annex] -> Annex ()
showTriedRemotes [] = return ()	
showTriedRemotes remotes =
	showLongNote $ "Unable to access these remotes: " ++
		(join ", " $ map name remotes)

forceTrust :: TrustLevel -> String -> Annex ()
forceTrust level remotename = do
	r <- nameToUUID remotename
	Annex.changeState $ \s ->
		s { Annex.forcetrust = (r, level):Annex.forcetrust s }

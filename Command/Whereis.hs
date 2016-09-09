{- git-annex command
 -
 - Copyright 2010-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Whereis where

import Command
import Remote
import Logs.Trust
import Logs.Web
import Remote.Web (getWebUrls)
import Annex.UUID

import qualified Data.Map as M

cmd :: Command
cmd = noCommit $ withGlobalOptions (jsonOption : annexedMatchingOptions) $
	command "whereis" SectionQuery
		"lists repositories that have file content"
		paramPaths (seek <$$> optParser)

data WhereisOptions = WhereisOptions
	{ whereisFiles :: CmdParams
	, keyOptions :: Maybe KeyOptions
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser WhereisOptions
optParser desc = WhereisOptions
	<$> cmdParams desc
	<*> optional parseKeyOptions
	<*> parseBatchOption

seek :: WhereisOptions -> CommandSeek
seek o = do
	m <- remoteMap id
	let go = whenAnnexed $ start m
	case batchOption o of
		Batch -> batchFiles go
		NoBatch -> 
			withKeyOptions (keyOptions o) False
				(startKeys m)
				(withFilesInGit go)
				(whereisFiles o)

start :: M.Map UUID Remote -> FilePath -> Key -> CommandStart
start remotemap file key = startKeys remotemap key (mkActionItem afile)
  where
	afile = Just file

startKeys :: M.Map UUID Remote -> Key -> ActionItem -> CommandStart
startKeys remotemap key ai = do
	showStart' "whereis" key ai
	next $ perform remotemap key

perform :: M.Map UUID Remote -> Key -> CommandPerform
perform remotemap key = do
	locations <- keyLocations key
	urls <- getUUIDUrls key locations remotemap
	(untrustedlocations, safelocations) <- trustPartition UnTrusted locations
	let num = length safelocations
	showNote $ show num ++ " " ++ copiesplural num
	pp <- ppwhereis "whereis" safelocations urls
	unless (null safelocations) $ showLongNote pp
	pp' <- ppwhereis "untrusted" untrustedlocations urls
	unless (null untrustedlocations) $ showLongNote $ untrustedheader ++ pp'

	mapM_ (showRemoteUrls remotemap) urls

	if null safelocations then stop else next $ return True
  where
	copiesplural 1 = "copy"
	copiesplural _ = "copies"
	untrustedheader = "The following untrusted locations may also have copies:\n"
	ppwhereis h ls urls = do
		descm <- uuidDescriptions
		let urlvals = map (\(u, us) -> (u, Just us)) $
			filter (\(u,_) -> u `elem` ls) urls
		prettyPrintUUIDsWith (Just "urls") h descm (const Nothing) urlvals

getUUIDUrls :: Key -> [UUID] -> M.Map UUID Remote -> Annex [(UUID, [URLString])]
getUUIDUrls key uuids remotemap = forM uuids $ \uu -> (,)
	<$> pure uu
	<*> maybe (pure []) (getRemoteUrls key) (M.lookup uu remotemap)

getRemoteUrls :: Key -> Remote -> Annex [URLString]
getRemoteUrls key remote
	| uuid remote == webUUID = getWebUrls key
	| otherwise = (++)
		<$> askremote
		<*> claimedurls
  where
	askremote = maybe (pure []) (flip id key) (whereisKey remote)
	claimedurls = do
		us <- map fst 
			. filter (\(_, d) -> d == OtherDownloader)
			. map getDownloader
			<$> getUrls key
		filterM (\u -> (==) <$> pure remote <*> claimingUrl u) us

showRemoteUrls :: M.Map UUID Remote -> (UUID, [URLString]) -> Annex ()
showRemoteUrls remotemap (uu, us)
	| null us = noop
	| otherwise = case M.lookup uu remotemap of
		Just r -> showLongNote $ 
			unlines $ map (\u -> name r ++ ": " ++ u) us 
		Nothing -> noop

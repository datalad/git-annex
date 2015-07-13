{- git-annex command
 -
 - Copyright 2010-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Whereis where

import qualified Data.Map as M

import Common.Annex
import Command
import Remote
import Logs.Trust
import Logs.Web

cmd :: Command
cmd = noCommit $ withGlobalOptions (jsonOption : annexedMatchingOptions) $
	command "whereis" SectionQuery
		"lists repositories that have file content"
		paramPaths (seek <$$> optParser)

data WhereisOptions = WhereisOptions
	{ whereisFiles :: CmdParams
	, keyOptions :: Maybe KeyOptions
	}

optParser :: CmdParamsDesc -> Parser WhereisOptions
optParser desc = WhereisOptions
	<$> cmdParams desc
	<*> optional (parseKeyOptions False)

seek :: WhereisOptions -> CommandSeek
seek o = do
	m <- remoteMap id
	withKeyOptions (keyOptions o) False
		(startKeys m)
		(withFilesInGit $ whenAnnexed $ start m)
		(whereisFiles o)

start :: M.Map UUID Remote -> FilePath -> Key -> CommandStart
start remotemap file key = start' remotemap key (Just file)

startKeys :: M.Map UUID Remote -> Key -> CommandStart
startKeys remotemap key = start' remotemap key Nothing

start' :: M.Map UUID Remote -> Key -> AssociatedFile -> CommandStart
start' remotemap key afile = do
	showStart' "whereis" key afile
	next $ perform remotemap key

perform :: M.Map UUID Remote -> Key -> CommandPerform
perform remotemap key = do
	locations <- keyLocations key
	(untrustedlocations, safelocations) <- trustPartition UnTrusted locations
	let num = length safelocations
	showNote $ show num ++ " " ++ copiesplural num
	pp <- prettyPrintUUIDs "whereis" safelocations
	unless (null safelocations) $ showLongNote pp
	pp' <- prettyPrintUUIDs "untrusted" untrustedlocations
	unless (null untrustedlocations) $ showLongNote $ untrustedheader ++ pp'
	forM_ (mapMaybe (`M.lookup` remotemap) locations) $
		performRemote key
	if null safelocations then stop else next $ return True
  where
	copiesplural 1 = "copy"
	copiesplural _ = "copies"
	untrustedheader = "The following untrusted locations may also have copies:\n"

performRemote :: Key -> Remote -> Annex () 
performRemote key remote = do
	ls <- (++)
		<$> askremote
		<*> claimedurls
	unless (null ls) $ showLongNote $ unlines $
		map (\l -> name remote ++ ": " ++ l) ls
  where
	askremote = maybe (pure []) (flip id key) (whereisKey remote)
	claimedurls = do
		us <- map fst 
			. filter (\(_, d) -> d == OtherDownloader)
			. map getDownloader
			<$> getUrls key
		filterM (\u -> (==) <$> pure remote <*> claimingUrl u) us

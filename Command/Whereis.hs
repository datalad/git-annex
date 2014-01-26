{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Whereis where

import qualified Data.Map as M

import Common.Annex
import Command
import Remote
import Logs.Trust
import GitAnnex.Options

def :: [Command]
def = [noCommit $ withOptions (jsonOption : keyOptions) $
	command "whereis" paramPaths seek SectionQuery
		"lists repositories that have file content"]

seek :: CommandSeek
seek ps = do
	m <- remoteMap id
	withKeyOptions
		(startKeys m)
		(withFilesInGit $ whenAnnexed $ start m)
		ps

start :: M.Map UUID Remote -> FilePath -> (Key, Backend) -> CommandStart
start remotemap file (key, _) = start' remotemap key (Just file)

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
performRemote key remote = maybe noop go $ whereisKey remote
  where
	go a = do
		ls <- a key
		unless (null ls) $ showLongNote $ unlines $
			map (\l -> name remote ++ ": " ++ l) ls

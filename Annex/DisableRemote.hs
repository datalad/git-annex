{- disable a remote
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.DisableRemote (disableRemote) where

import Git
import Annex.Common
import qualified Annex
import qualified Git.Remote.Remove
import qualified Git.Ref
import Types.Remote
import Annex.Journal
import qualified Annex.Branch
import Annex.Branch.Transitions
import Types.Transitions
import Logs
import Logs.Remote.Pure
import Logs.MapLog
import Logs.Transfer
import Types.Transfer
import qualified Database.Export
import qualified Database.Fsck
import qualified Database.RepoSize
import qualified Database.ContentIdentifier
import qualified Utility.OsString as OS

import Data.ByteString.Builder
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as L

disableRemote :: Remote -> RemoteName -> [Remote] -> Annex ()
disableRemote r remotename remotelist = do
	let uniqueuuid = not $
		any (\r' -> uuid r' == uuid r && name r' /= name r) remotelist
	
	when uniqueuuid $
		-- If there are transfers to/from the remote still
		-- running, this fails. That's why it's run early.
		removeTransferLogs (uuid r)
		
	cleanPrivateJournal r uniqueuuid

	when uniqueuuid $ do
		-- Each uuid has its own export and fsck database,
		-- so always remove them, so long as this is the
		-- only remote using this uuid.
		Database.Export.removeDb (uuid r)
		Database.Fsck.removeDb (uuid r)
		-- These databases are updated from information
		-- in the git-annex branch, so there is no point in
		-- removing the uuid from them unless it's private.
		whenM (isPrivateUUID (uuid r)) $ do
			Database.RepoSize.removeUUID (uuid r)
			Database.ContentIdentifier.removeUUID (uuid r) True

		removeFsckState (uuid r)
		removeImportLog (uuid r)
		removeCredFiles (uuid r)
		removeRemoteLockFile (uuid r)
		
	inRepo $ Git.Remote.Remove.remove remotename
	removeRemoteTrackingBranches remotename

-- Remove any remote branches. 
-- This is done because git remote remove only removes the configured
-- remote tracking branch, not other remote branches.
removeRemoteTrackingBranches :: String -> Annex ()
removeRemoteTrackingBranches remotename = do
	branches <- filter (\b -> branchprefix `isPrefixOf` fromRef b) 
		. map snd
		<$> inRepo Git.Ref.list
	forM_ branches $ \b -> 
		inRepo $ Git.Ref.delete' b
  where
	branchprefix = "refs/remotes/" ++ remotename ++ "/"

isPrivateUUID :: UUID -> Annex Bool
isPrivateUUID u = 
	(\c -> u `S.member` annexPrivateRepos c)
		<$> Annex.getGitConfig

{- Remove a private remote's uuid from the private journal
 - entirely when it is the only remote using that uuid.
 -
 - And, when the remote is a sameas remote, its config is stored
 - under its config-uuid. Remove that from the private remote log.
 -}
cleanPrivateJournal :: Remote -> Bool -> Annex ()
cleanPrivateJournal r uniqueuuid
	| uniqueuuid == True = do
		whenM (isPrivateUUID (uuid r)) $ do
			gc <- Annex.getGitConfig
			let tc = filterBranch (\u -> u /= uuid r) gc
			let handlestale = \_ b -> return (b, Nothing)
			lockJournal $ \jl -> 
				overPrivateJournalFileContents handlestale Just
					(go jl tc)
		removeconfiguuid
	| otherwise = removeconfiguuid
  where
	go jl tc getfilecontents = getfilecontents >>= \case
		Just (_, p, Just (b, _)) -> do
			cleaner jl tc p b
			go jl tc getfilecontents
		Just (_, _, Nothing) ->
			go jl tc getfilecontents
		Nothing -> return ()
			
	cleaner jl tc p b = case tc p b of
		PreserveFile -> return ()
		ChangeFile builder ->
			setlocaljournal jl (RegardingUUID [uuid r]) p builder
	
	removeconfiguuid = case remoteAnnexConfigUUID (gitconfig r) of
		Nothing -> return ()
		Just cu -> whenM (isPrivateUUID cu) $
			lockJournal $ \jl ->
				getJournalFile jl (GetPrivate True) remoteLog >>= \case
					JournalledContent b -> 
						scrub jl cu b
					PossiblyStaleJournalledContent b -> 
						scrub jl cu b
					NoJournalledContent ->
						return ()
	  where
		scrub jl cu b =
			setlocaljournal jl (RegardingUUID [cu]) remoteLog $
				buildRemoteConfigLog $ MapLog $ M.delete cu $
					fromMapLog $ parseRemoteConfigLog b

	setlocaljournal jl ru p builder =
		let b' = toLazyByteString builder
		in if L.null b'
			then deleteJournalFile jl ru p
			else do
				b'' <- Annex.Branch.getLocal' (GetPrivate False) p
				if b'' == b'
					then deleteJournalFile jl ru p
					else setJournalFile jl ru p builder

removeFsckState :: UUID -> Annex ()
removeFsckState u = do
	d <- fromRepo (gitAnnexFsckStateDir u)
	liftIO $ whenM (doesDirectoryExist d) $
		removeDirectoryRecursive d
	f <- fromRepo (gitAnnexFsckResultsLog u)
	liftIO $ removeWhenExistsWith removeFile f

removeImportLog :: UUID -> Annex ()
removeImportLog u = do
	f <- calcRepo' (gitAnnexImportLog u)
	liftIO $ removeWhenExistsWith removeFile f
	
removeTransferLogs :: UUID -> Annex ()
removeTransferLogs u = do
	-- getTransfers calls checkTransfer, which cleans up
	-- transfer log files for transfers that are no longer running.
	whenM (any foru <$> getTransfers) $
		error "Active trasfers, cannot disable the remote."
	forM_ [Upload, Download] $ \direction -> do
		d <- fromRepo $ gitAnnexTransferUUIDDirectionDir u direction
		liftIO $ void $ tryNonAsync $ removeDirectory d
		d' <- fromRepo $ gitAnnexFailedTransferDir u direction
		liftIO $ void $ tryNonAsync $ removeDirectory d'
	clearFailedTransfers u
  where
	foru (t, _) = transferUUID t == u

removeCredFiles :: UUID -> Annex ()
removeCredFiles u = do
	d <- fromRepo gitAnnexCredsDir
	liftIO $ whenM (doesDirectoryExist d) $ do
		mapM_ (removeWhenExistsWith removeFile)
			=<< filter foru <$> dirContents d
  where
	us = fromUUID u

	-- Remotes that use creds always include their UUID as part of the
	-- filename. However, some remotes (Remote.External) need more than
	-- one creds file, and add a "-foo" suffix to the UUID.
	foru p = 
		let f = takeFileName p
		in f == us || (us <> literalOsPath "-") `OS.isPrefixOf` f

removeRemoteLockFile :: UUID -> Annex ()
removeRemoteLockFile u = do
	lck <- fromRepo $ gitAnnexRemoteLockFile u
	liftIO $ removeWhenExistsWith removeFile lck


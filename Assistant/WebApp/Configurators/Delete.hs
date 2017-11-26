{- git-annex assistant webapp repository deletion
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Configurators.Delete where

import Assistant.WebApp.Common
import Assistant.DeleteRemote
import Assistant.DaemonStatus
import Assistant.ScanRemotes
import Assistant.Sync
import qualified Remote
import qualified Git
import Config.Files
import Logs.Trust
import Logs.Remote
import Logs.PreferredContent
import Types.StandardGroups
import Annex.UUID
import Command.Uninit (prepareRemoveAnnexDir)

import qualified Data.Text as T
import qualified Data.Map as M

notCurrentRepo :: UUID -> Handler Html -> Handler Html
notCurrentRepo uuid a = do
	u <- liftAnnex getUUID
	if u == uuid
		then redirect DeleteCurrentRepositoryR
		else go =<< liftAnnex (Remote.remoteFromUUID uuid)
  where
	go Nothing = error "Unknown UUID"
	go (Just _) = a

getDeleteRepositoryR :: UUID -> Handler Html
getDeleteRepositoryR uuid = notCurrentRepo uuid $ do
	deletionPage $ do
		reponame <- liftAnnex $ Remote.prettyUUID uuid
		$(widgetFile "configurators/delete/start")

getStartDeleteRepositoryR :: UUID -> Handler Html
getStartDeleteRepositoryR uuid = do
	remote <- fromMaybe (error "unknown remote")
		<$> liftAnnex (Remote.remoteFromUUID uuid)
	liftAnnex $ do
		trustSet uuid UnTrusted
		setStandardGroup uuid UnwantedGroup
	liftAssistant $ addScanRemotes True [remote]
	redirect DashboardR

getFinishDeleteRepositoryR :: UUID -> Handler Html
getFinishDeleteRepositoryR uuid = deletionPage $ do
	void $ liftAssistant $ removeRemote uuid

	reponame <- liftAnnex $ Remote.prettyUUID uuid
	{- If it's not listed in the remote log, it must be a git repo. -}
	gitrepo <- liftAnnex $ M.notMember uuid <$> readRemoteLog
	$(widgetFile "configurators/delete/finished")	

getDeleteCurrentRepositoryR :: Handler Html
getDeleteCurrentRepositoryR = deleteCurrentRepository

postDeleteCurrentRepositoryR :: Handler Html
postDeleteCurrentRepositoryR = deleteCurrentRepository

deleteCurrentRepository :: Handler Html
deleteCurrentRepository = dangerPage $ do
	reldir <- fromJust . relDir <$> liftH getYesod
	havegitremotes <- haveremotes syncGitRemotes
	havedataremotes <- haveremotes downloadRemotes
	((result, form), enctype) <- liftH $
		runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $
			sanityVerifierAForm $ SanityVerifier magicphrase
	case result of
		FormSuccess _ -> liftH $ do
			dir <- liftAnnex $ fromRepo Git.repoPath
			liftIO $ removeAutoStartFile dir

			{- Disable syncing to this repository, and all
			 - remotes. This stops all transfers, and all
			 - file watching. -}
			liftAssistant $ do
				changeSyncable Nothing False
				rs <- syncRemotes <$> getDaemonStatus
				mapM_ (\r -> changeSyncable (Just r) False) rs

			liftAnnex $ prepareRemoveAnnexDir dir
			liftIO $ removeDirectoryRecursive =<< absPath dir
			
			redirect ShutdownConfirmedR
		_ -> $(widgetFile "configurators/delete/currentrepository")
  where
	haveremotes selector = not . null . selector
		<$> liftAssistant getDaemonStatus

data SanityVerifier = SanityVerifier T.Text
	deriving (Eq)

sanityVerifierAForm :: SanityVerifier -> MkAForm SanityVerifier
sanityVerifierAForm template = SanityVerifier
	<$> areq checksanity (bfs "Confirm deletion?") Nothing
  where
	checksanity = checkBool (\input -> SanityVerifier input == template)
		insane textField
	
	insane = "Maybe this is not a good idea..." :: Text

deletionPage :: Widget -> Handler Html
deletionPage = page "Delete repository" (Just Configuration)

dangerPage :: Widget -> Handler Html
dangerPage = page "Danger danger danger" (Just Configuration)

magicphrase :: Text
magicphrase = "Yes, please do as I say!"

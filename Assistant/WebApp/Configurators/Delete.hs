{- git-annex assistant webapp repository deletion
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Configurators.Delete where

import Assistant.WebApp.Common
import Assistant.DeleteRemote
import Assistant.WebApp.Utility
import Assistant.DaemonStatus
import Assistant.ScanRemotes
import qualified Remote
import qualified Git
import Config.Files
import Utility.FileMode
import Logs.Trust
import Logs.Remote
import Logs.PreferredContent
import Types.StandardGroups

import System.IO.HVFS (SystemFS(..))
import qualified Data.Text as T
import qualified Data.Map as M
import System.Path

notCurrentRepo :: UUID -> Handler RepHtml -> Handler RepHtml
notCurrentRepo uuid a = go =<< liftAnnex (Remote.remoteFromUUID uuid)
  where
  	go Nothing = redirect DeleteCurrentRepositoryR
	go (Just _) = a

getDisableRepositoryR :: UUID -> Handler RepHtml
getDisableRepositoryR uuid = notCurrentRepo uuid $ do
	void $ liftAssistant $ disableRemote uuid
	redirect DashboardR

getDeleteRepositoryR :: UUID -> Handler RepHtml
getDeleteRepositoryR uuid = notCurrentRepo uuid $
	deletionPage $ do
		reponame <- liftAnnex $ Remote.prettyUUID uuid
		$(widgetFile "configurators/delete/start")

getStartDeleteRepositoryR :: UUID -> Handler RepHtml
getStartDeleteRepositoryR uuid = do
	remote <- fromMaybe (error "unknown remote")
		<$> liftAnnex (Remote.remoteFromUUID uuid)
	liftAnnex $ do
		trustSet uuid UnTrusted
		setStandardGroup uuid UnwantedGroup
	liftAssistant $ addScanRemotes True [remote]
	redirect DashboardR

getFinishDeleteRepositoryR :: UUID -> Handler RepHtml
getFinishDeleteRepositoryR uuid = deletionPage $ do
	void $ liftAssistant $ removeRemote uuid

	reponame <- liftAnnex $ Remote.prettyUUID uuid
	{- If it's not listed in the remote log, it must be a git repo. -}
	gitrepo <- liftAnnex $ M.notMember uuid <$> readRemoteLog
	$(widgetFile "configurators/delete/finished")	

getDeleteCurrentRepositoryR :: Handler RepHtml
getDeleteCurrentRepositoryR = deleteCurrentRepository

postDeleteCurrentRepositoryR :: Handler RepHtml
postDeleteCurrentRepositoryR = deleteCurrentRepository

deleteCurrentRepository :: Handler RepHtml
deleteCurrentRepository = dangerPage $ do
	reldir <- fromJust . relDir <$> liftH getYesod
	havegitremotes <- haveremotes syncGitRemotes
	havedataremotes <- haveremotes syncDataRemotes
	((result, form), enctype) <- liftH $
		runFormPost $ renderBootstrap $ sanityVerifierAForm $
			SanityVerifier magicphrase
	case result of
		FormSuccess _ -> liftH $ do
			dir <- liftAnnex $ fromRepo Git.repoPath
			liftIO $ removeAutoStartFile dir

			{- Disable syncing to this repository, and all
			 - remotes. This stops all transfers, and all
			 - file watching. -}
			changeSyncable Nothing False
			rs <- liftAssistant $ syncRemotes <$> getDaemonStatus
			mapM_ (\r -> changeSyncable (Just r) False) rs

			{- Make all directories writable, so all annexed
			 - content can be deleted. -}
			liftIO $ do
				recurseDir SystemFS dir >>=
					filterM doesDirectoryExist >>=
						mapM_ allowWrite
				removeDirectoryRecursive dir
			
			redirect ShutdownConfirmedR
		_ -> $(widgetFile "configurators/delete/currentrepository")
  where
	haveremotes selector = not . null . selector
		<$> liftAssistant getDaemonStatus

data SanityVerifier = SanityVerifier T.Text
	deriving (Eq)

sanityVerifierAForm :: SanityVerifier -> MkAForm SanityVerifier
sanityVerifierAForm template = SanityVerifier
	<$> areq checksanity "Confirm deletion?" Nothing
  where
	checksanity = checkBool (\input -> SanityVerifier input == template)
		insane textField
	
	insane = "Maybe this is not a good idea..." :: Text

deletionPage :: Widget -> Handler RepHtml
deletionPage = page "Delete repository" (Just Configuration)

dangerPage :: Widget -> Handler RepHtml
dangerPage = page "Danger danger danger" (Just Configuration)

magicphrase :: Text
magicphrase = "Yes, please do as I say!"

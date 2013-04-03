{- git-annex assistant webapp repository deletion
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.Delete where

import Assistant.WebApp.Common
import Assistant.DeleteRemote
import Assistant.WebApp.Utility
import Assistant.DaemonStatus
import Assistant.ScanRemotes
import qualified Remote
import qualified Git
import Locations.UserConfig
import Utility.FileMode
import Logs.Trust
import Logs.Remote
import Logs.PreferredContent
import Types.StandardGroups

import System.IO.HVFS (SystemFS(..))
import qualified Data.Text as T
import qualified Data.Map as M

getDeleteRepositoryR :: UUID -> Handler RepHtml
getDeleteRepositoryR uuid = go =<< liftAnnex (Remote.remoteFromUUID uuid)
  where
  	go Nothing = redirect DeleteCurrentRepositoryR
	go (Just r) = deletionPage $ do
		reponame <- liftAnnex $ Remote.prettyUUID uuid
		$(widgetFile "configurators/delete/choose")

getDeleteRepositoryFromListR :: UUID -> Handler RepHtml
getDeleteRepositoryFromListR uuid = do
	void $ liftAssistant $ removeRemote uuid
	redirect DashboardR

getStartDeleteRepositoryContentsR :: UUID -> Handler RepHtml
getStartDeleteRepositoryContentsR uuid = deletionPage $ do
	remote <- fromMaybe (error "unknown remote")
		<$> liftAnnex (Remote.remoteFromUUID uuid)
	liftAnnex $ do
		trustSet uuid UnTrusted
		setStandardGroup uuid UnwantedGroup
	liftAssistant $ addScanRemotes True [remote]

	reponame <- liftAnnex $ Remote.prettyUUID uuid
	$(widgetFile "configurators/delete/started")

getFinishedDeletingRepositoryContentsR :: UUID -> Handler RepHtml
getFinishedDeletingRepositoryContentsR uuid = deletionPage $ do
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
	reldir <- fromJust . relDir <$> lift getYesod
	havegitremotes <- haveremotes syncGitRemotes
	havedataremotes <- haveremotes syncDataRemotes
	((result, form), enctype) <- lift $
		runFormPost $ renderBootstrap $ sanityVerifierAForm $
			SanityVerifier magicphrase
	case result of
		FormSuccess _ -> lift $ do
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

sanityVerifierAForm :: SanityVerifier -> AForm WebApp WebApp SanityVerifier
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

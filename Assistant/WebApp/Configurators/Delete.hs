{- git-annex assistant webapp repository deletion
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.Delete where

import Assistant.WebApp.Common
import Assistant.WebApp.Utility
import Assistant.DaemonStatus
import qualified Remote
import qualified Types.Remote as Remote
import Remote.List (remoteListRefresh)
import Command.Dead (markDead)
import qualified Git.Command
import qualified Git
import Locations.UserConfig
import Utility.FileMode

import qualified Data.Text as T
import System.IO.HVFS (SystemFS(..))

getDeleteRepositoryR :: UUID -> Handler RepHtml
getDeleteRepositoryR uuid = go =<< liftAnnex (Remote.remoteFromUUID uuid)
  where
  	go Nothing = redirect DeleteCurrentRepositoryR
	go (Just r) = page "Delete repository" (Just Configuration) $ do
		reponame <- liftAnnex $ concat <$> Remote.prettyListUUIDs [uuid]
		$(widgetFile "configurators/delete/choose")

getDeleteRepositoryFromListR :: UUID -> Handler RepHtml
getDeleteRepositoryFromListR uuid = do
	void $ liftAssistant $ removeRemote uuid
	redirect DashboardR

removeRemote :: UUID -> Assistant Remote
removeRemote uuid = do
	remote <- fromMaybe (error "unknown remote")
		<$> liftAnnex (Remote.remoteFromUUID uuid)
	liftAnnex $ do
		inRepo $ Git.Command.run
			[ Param "remote"
			, Param "remove"
			, Param (Remote.name remote)
			]
		void $ remoteListRefresh
	updateSyncRemotes
	return remote

getDeleteRepositoryContentsR :: UUID -> Handler RepHtml
getDeleteRepositoryContentsR = deleteRepositoryContents

postDeleteRepositoryContentsR :: UUID -> Handler RepHtml
postDeleteRepositoryContentsR = deleteRepositoryContents

deleteRepositoryContents :: UUID -> Handler RepHtml
deleteRepositoryContents uuid = dangerPage $ do
	reponame <- liftAnnex $ concat <$> Remote.prettyListUUIDs [uuid]
	((result, form), enctype) <- lift $
		runFormPost $ renderBootstrap $ sanityVerifierAForm $
			SanityVerifier magicphrase
	case result of
		FormSuccess _ -> do
			liftAnnex $ markDead uuid
			oldremote <- liftAssistant $ removeRemote uuid
			$(widgetFile "configurators/delete/repositorycontents/nextstep")
		_ -> $(widgetFile "configurators/delete/repositorycontents")

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

dangerPage :: Widget -> Handler RepHtml
dangerPage = page "Danger danger danger" (Just Configuration)

magicphrase :: Text
magicphrase = "Yes, please do as I say!"

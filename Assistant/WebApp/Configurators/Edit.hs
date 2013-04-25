{- git-annex assistant webapp configurator for editing existing repos
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.Edit where

import Assistant.WebApp.Common
import Assistant.WebApp.Utility
import Assistant.DaemonStatus
import Assistant.MakeRemote (uniqueRemoteName)
import Assistant.WebApp.Configurators.XMPP (xmppNeeded)
import Assistant.ScanRemotes
import qualified Remote
import qualified Types.Remote as Remote
import qualified Remote.List as Remote
import Logs.UUID
import Logs.Group
import Logs.PreferredContent
import Types.StandardGroups
import qualified Git
import qualified Git.Command
import qualified Git.Config
import qualified Annex
import Git.Remote

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

data RepoGroup = RepoGroupCustom String | RepoGroupStandard StandardGroup
	deriving (Show, Eq)

data RepoConfig = RepoConfig
	{ repoName :: Text
	, repoDescription :: Maybe Text
	, repoGroup :: RepoGroup
	, repoSyncable :: Bool
	}
	deriving (Show)

getRepoConfig :: UUID -> Maybe Remote -> Annex RepoConfig
getRepoConfig uuid mremote = RepoConfig
	<$> pure (T.pack $ maybe "here" Remote.name mremote)
	<*> (maybe Nothing (Just . T.pack) . M.lookup uuid <$> uuidMap)
	<*> getrepogroup
	<*> getsyncing
  where
	getrepogroup = do
		groups <- lookupGroups uuid
		return $ 
			maybe (RepoGroupCustom $ unwords $ S.toList groups) RepoGroupStandard
				(getStandardGroup groups)
	getsyncing = case mremote of
		Just r -> return $ remoteAnnexSync $ Remote.gitconfig r
		Nothing -> annexAutoCommit <$> Annex.getGitConfig

setRepoConfig :: UUID -> Maybe Remote -> RepoConfig -> RepoConfig -> Handler ()
setRepoConfig uuid mremote oldc newc = do
	when descriptionChanged $ liftAnnex $ do
		maybe noop (describeUUID uuid . T.unpack) (repoDescription newc)
		void uuidMapLoad
	when nameChanged $ do
		liftAnnex $ do
			name <- fromRepo $ uniqueRemoteName (legalName newc) 0
			{- git remote rename expects there to be a
			 - remote.<name>.fetch, and exits nonzero if
			 - there's not. Special remotes don't normally
			 - have that, and don't use it. Temporarily add
			 - it if it's missing. -}
			let remotefetch = "remote." ++ T.unpack (repoName oldc) ++ ".fetch"
			needfetch <- isNothing <$> fromRepo (Git.Config.getMaybe remotefetch)
			when needfetch $
				inRepo $ Git.Command.run
					[Param "config", Param remotefetch, Param ""]
			inRepo $ Git.Command.run
				[ Param "remote"
				, Param "rename"
				, Param $ T.unpack $ repoName oldc
				, Param name
				]
			void $ Remote.remoteListRefresh
		liftAssistant updateSyncRemotes
	when groupChanged $ do
		liftAnnex $ case repoGroup newc of
			RepoGroupStandard g -> setStandardGroup uuid g
			RepoGroupCustom s -> groupSet uuid $ S.fromList $ words s
		{- Enabling syncing will cause a scan,
		 - so avoid queueing a duplicate scan. -}
		when (repoSyncable newc && not syncableChanged) $ liftAssistant $
			case mremote of
				Just remote -> do
					addScanRemotes True [remote]
				Nothing -> do
					addScanRemotes True
						=<< syncDataRemotes <$> getDaemonStatus
	when syncableChanged $
		changeSyncable mremote (repoSyncable newc)
  where
  	syncableChanged = repoSyncable oldc /= repoSyncable newc
	groupChanged = repoGroup oldc /= repoGroup newc
	nameChanged = isJust mremote && legalName oldc /= legalName newc
	descriptionChanged = repoDescription oldc /= repoDescription newc

	legalName = makeLegalName . T.unpack . repoName

editRepositoryAForm :: RepoConfig -> AForm WebApp WebApp RepoConfig
editRepositoryAForm def = RepoConfig
	<$> areq textField "Name" (Just $ repoName def)
	<*> aopt textField "Description" (Just $ repoDescription def)
	<*> areq (selectFieldList groups `withNote` help) "Repository group" (Just $ repoGroup def)
	<*> areq checkBoxField "Syncing enabled" (Just $ repoSyncable def)
  where
	groups = customgroups ++ standardgroups
	standardgroups :: [(Text, RepoGroup)]
	standardgroups = map (\g -> (T.pack $ descStandardGroup g , RepoGroupStandard g))
		[minBound :: StandardGroup .. maxBound :: StandardGroup]
	customgroups :: [(Text, RepoGroup)]
	customgroups = case repoGroup def of
		RepoGroupCustom s -> [(T.pack s, RepoGroupCustom s)]
		_ -> []
	help = [whamlet|<a href="@{RepoGroupR}">What's this?</a>|]

getEditRepositoryR :: UUID -> Handler RepHtml
getEditRepositoryR = postEditRepositoryR

postEditRepositoryR :: UUID -> Handler RepHtml
postEditRepositoryR = editForm False

getEditNewRepositoryR :: UUID -> Handler RepHtml
getEditNewRepositoryR = postEditNewRepositoryR

postEditNewRepositoryR :: UUID -> Handler RepHtml
postEditNewRepositoryR = editForm True

getEditNewCloudRepositoryR :: UUID -> Handler RepHtml
getEditNewCloudRepositoryR = postEditNewCloudRepositoryR

postEditNewCloudRepositoryR :: UUID -> Handler RepHtml
postEditNewCloudRepositoryR uuid = xmppNeeded >> editForm True uuid

editForm :: Bool -> UUID -> Handler RepHtml
editForm new uuid = page "Configure repository" (Just Configuration) $ do
	mremote <- liftAnnex $ Remote.remoteFromUUID uuid
	curr <- liftAnnex $ getRepoConfig uuid mremote
	lift $ checkdirectories curr
	((result, form), enctype) <- lift $
		runFormPost $ renderBootstrap $ editRepositoryAForm curr
	case result of
		FormSuccess input -> lift $ do
			checkdirectories input
			setRepoConfig uuid mremote curr input
			redirect DashboardR
		_ -> showform form enctype curr
  where
	showform form enctype curr = do
		let istransfer = repoGroup curr == RepoGroupStandard TransferGroup
		$(widgetFile "configurators/editrepository")

	{- Makes a toplevel archive or public directory, so the user can
	 - get on with using it. This is done both when displaying the form,
	 - as well as after it's posted, because the user may not post the form,
	 - but may see that the repo is set up to use the archive
	 - directory. -}
	checkdirectories cfg
		| repoGroup cfg == RepoGroupStandard SmallArchiveGroup = go "archive"
		| repoGroup cfg == RepoGroupStandard FullArchiveGroup = go "archive"
		| repoGroup cfg == RepoGroupStandard PublicGroup = go "public"
		| otherwise = noop
	  where
		go d = liftAnnex $ inRepo $ \g ->
			createDirectoryIfMissing True $
				Git.repoPath g </> d

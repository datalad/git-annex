{- git-annex assistant webapp configurator for editing existing repos
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Assistant.WebApp.Configurators.Edit where

import Assistant.WebApp.Common
import Assistant.WebApp.Gpg
import Assistant.WebApp.Configurators
import Assistant.DaemonStatus
import Assistant.WebApp.MakeRemote (uniqueRemoteName)
import Assistant.ScanRemotes
import Assistant.Sync
import Assistant.Alert
import qualified Assistant.WebApp.Configurators.AWS as AWS
#ifdef WITH_S3
import qualified Assistant.WebApp.Configurators.IA as IA
import qualified Remote.S3 as S3
#endif
import qualified Remote
import qualified Types.Remote as Remote
import qualified Remote.List as Remote
import Logs.UUID
import Logs.Group
import Logs.PreferredContent
import Logs.Remote
import Types.StandardGroups
import qualified Git
import qualified Git.Types as Git
import qualified Git.Command
import qualified Git.Config
import qualified Annex
import Git.Remote
import Remote.Helper.Encryptable (extractCipher)
import Types.Crypto
import Utility.Gpg
import Annex.UUID
import Assistant.Ssh
import Config

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

data RepoGroup = RepoGroupCustom String | RepoGroupStandard StandardGroup
	deriving (Show, Eq)

data RepoConfig = RepoConfig
	{ repoName :: Text
	, repoDescription :: Maybe Text
	, repoGroup :: RepoGroup
	, repoAssociatedDirectory :: Maybe Text
	, repoSyncable :: Bool
	}
	deriving (Show)

getRepoConfig :: UUID -> Maybe Remote -> Annex RepoConfig
getRepoConfig uuid mremote = do
	-- Ensure we're editing current data by discarding caches.
	void groupMapLoad
	void uuidMapLoad

	groups <- lookupGroups uuid
	remoteconfig <- M.lookup uuid <$> readRemoteLog
	let (repogroup, associateddirectory) = case getStandardGroup groups of
		Nothing -> (RepoGroupCustom $ unwords $ S.toList groups, Nothing)
		Just g -> (RepoGroupStandard g, associatedDirectory remoteconfig g)
	
	description <- fmap T.pack . M.lookup uuid <$> uuidMap

	syncable <- case mremote of
		Just r -> return $ remoteAnnexSync $ Remote.gitconfig r
		Nothing -> annexAutoCommit <$> Annex.getGitConfig

	return $ RepoConfig
		(T.pack $ maybe "here" Remote.name mremote)
		description
		repogroup
		(T.pack <$> associateddirectory)
		syncable
		
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
			void Remote.remoteListRefresh
		liftAssistant updateSyncRemotes
	when associatedDirectoryChanged $ case repoAssociatedDirectory newc of
		Nothing -> noop
		Just t
			| T.null t -> noop
			| otherwise -> liftAnnex $ do
				let dir = takeBaseName $ T.unpack t
				m <- readRemoteLog
				case M.lookup uuid m of
					Nothing -> noop
					Just remoteconfig -> configSet uuid $
						M.insert "preferreddir" dir remoteconfig
	when groupChanged $ do
		liftAnnex $ case repoGroup newc of
			RepoGroupStandard g -> setStandardGroup uuid g
			RepoGroupCustom s -> groupSet uuid $ S.fromList $ words s
		{- Enabling syncing will cause a scan,
		 - so avoid queueing a duplicate scan. -}
		when (repoSyncable newc && not syncableChanged) $ liftAssistant $
			case mremote of
				Just remote -> addScanRemotes True [remote]
				Nothing -> addScanRemotes True
					=<< syncDataRemotes <$> getDaemonStatus
	when syncableChanged $
		liftAssistant $ changeSyncable mremote (repoSyncable newc)
  where
	syncableChanged = repoSyncable oldc /= repoSyncable newc
	associatedDirectoryChanged = repoAssociatedDirectory oldc /= repoAssociatedDirectory newc
	groupChanged = repoGroup oldc /= repoGroup newc
	nameChanged = isJust mremote && legalName oldc /= legalName newc
	descriptionChanged = repoDescription oldc /= repoDescription newc

	legalName = makeLegalName . T.unpack . repoName

editRepositoryAForm :: Maybe Remote -> RepoConfig -> MkAForm RepoConfig
editRepositoryAForm mremote d = RepoConfig
	<$> areq (if ishere then readonlyTextField else textField)
		(bfs "Name") (Just $ repoName d)
	<*> aopt textField (bfs "Description") (Just $ repoDescription d)
	<*> areq (selectFieldList groups `withNote` help) (bfs "Repository group") (Just $ repoGroup d)
	<*> associateddirectory
	<*> areq checkBoxField "Syncing enabled" (Just $ repoSyncable d)
  where
	ishere = isNothing mremote
	isspecial = fromMaybe False $
		(== Git.Unknown) . Git.location . Remote.repo <$> mremote
	groups = customgroups ++ standardgroups
	standardgroups :: [(Text, RepoGroup)]
	standardgroups = map (\g -> (T.pack $ descStandardGroup g , RepoGroupStandard g)) $
		filter sanegroup [minBound..maxBound]
	sanegroup
		| isspecial = const True
		| otherwise = not . specialRemoteOnly
	customgroups :: [(Text, RepoGroup)]
	customgroups = case repoGroup d of
		RepoGroupCustom s -> [(T.pack s, RepoGroupCustom s)]
		_ -> []
	help = [whamlet|<a href="@{RepoGroupR}">What's this?</a>|]

	associateddirectory = case repoAssociatedDirectory d of
		Nothing -> aopt hiddenField "" Nothing
		Just dir -> aopt textField (bfs "Associated directory") (Just $ Just dir)

getEditRepositoryR :: RepoId -> Handler Html
getEditRepositoryR = postEditRepositoryR

postEditRepositoryR :: RepoId -> Handler Html
postEditRepositoryR = editForm False

getEditNewRepositoryR :: UUID -> Handler Html
getEditNewRepositoryR = postEditNewRepositoryR

postEditNewRepositoryR :: UUID -> Handler Html
postEditNewRepositoryR = editForm True . RepoUUID

getEditNewCloudRepositoryR :: UUID -> Handler Html
getEditNewCloudRepositoryR = postEditNewCloudRepositoryR

postEditNewCloudRepositoryR :: UUID -> Handler Html
postEditNewCloudRepositoryR uuid = connectionNeeded >> editForm True (RepoUUID uuid)

editForm :: Bool -> RepoId -> Handler Html
editForm new (RepoUUID uuid)
	| uuid == webUUID || uuid == bitTorrentUUID = page "The web" (Just Configuration) $ do
		$(widgetFile "configurators/edit/webrepository")
	| otherwise = page "Edit repository" (Just Configuration) $ do
		mremote <- liftAnnex $ Remote.remoteFromUUID uuid
		when (mremote == Nothing) $
			whenM ((/=) uuid <$> liftAnnex getUUID) $
				error "unknown remote"
		curr <- liftAnnex $ getRepoConfig uuid mremote
		liftAnnex $ checkAssociatedDirectory curr mremote
		((result, form), enctype) <- liftH $
			runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $ editRepositoryAForm mremote curr
		case result of
			FormSuccess input -> liftH $ do
				setRepoConfig uuid mremote curr input
				liftAnnex $ checkAssociatedDirectory input mremote
				redirect DashboardR
			_ -> do
				let istransfer = repoGroup curr == RepoGroupStandard TransferGroup
				config <- liftAnnex $ M.lookup uuid <$> readRemoteLog
				let repoInfo = getRepoInfo mremote config
				let repoEncryption = getRepoEncryption mremote config
				$(widgetFile "configurators/edit/repository")
editForm _new r@(RepoName _) = page "Edit repository" (Just Configuration) $ do
	mr <- liftAnnex (repoIdRemote r)
	let repoInfo = getRepoInfo mr Nothing
	g <- liftAnnex gitRepo
	let sshrepo = maybe False (remoteLocationIsSshUrl . flip parseRemoteLocation g . Git.repoLocation . Remote.repo) mr
	$(widgetFile "configurators/edit/nonannexremote")

{- Makes any directory associated with the repository. -}
checkAssociatedDirectory :: RepoConfig -> Maybe Remote -> Annex ()
checkAssociatedDirectory _ Nothing = noop
checkAssociatedDirectory cfg (Just r) = do
	repoconfig <- M.lookup (Remote.uuid r) <$> readRemoteLog
	case repoGroup cfg of
		RepoGroupStandard gr -> case associatedDirectory repoconfig gr of
			Just d -> inRepo $ \g ->
				createDirectoryIfMissing True $
					Git.repoPath g </> d
			Nothing -> noop
		_ -> noop

getRepoInfo :: Maybe Remote.Remote -> Maybe Remote.RemoteConfig -> Widget
getRepoInfo (Just r) (Just c) = case M.lookup "type" c of
	Just "S3"
#ifdef WITH_S3
		| S3.configIA c -> IA.getRepoInfo c
#endif
		| otherwise -> AWS.getRepoInfo c
	Just t
		| t /= "git" -> [whamlet|#{t} remote|]
	_ -> getGitRepoInfo $ Remote.repo r
getRepoInfo (Just r) _ = getRepoInfo (Just r) (Just $ Remote.config r)
getRepoInfo _ _ = [whamlet|git repository|]

getGitRepoInfo :: Git.Repo -> Widget
getGitRepoInfo r = do
	let loc = Git.repoLocation r
	[whamlet|git repository located at <tt>#{loc}</tt>|]

getRepoEncryption :: Maybe Remote.Remote -> Maybe Remote.RemoteConfig -> Widget
getRepoEncryption (Just _) (Just c) = case extractCipher c of
	Nothing ->
		[whamlet|not encrypted|]
	(Just (SharedCipher _)) ->
		[whamlet|encrypted: encryption key stored in git repository|]
	(Just (EncryptedCipher _ _ (KeyIds { keyIds = ks }))) -> do
		cmd <- liftAnnex $ gpgCmd <$> Annex.getGitConfig
		knownkeys <- liftIO (secretKeys cmd)
		[whamlet|
encrypted using gpg key:
<ul style="list-style: none">
  $forall k <- ks
    <li>
      ^{gpgKeyDisplay k (M.lookup k knownkeys)}
|]
getRepoEncryption _ _ = return () -- local repo

getUpgradeRepositoryR  :: RepoId -> Handler ()
getUpgradeRepositoryR (RepoUUID _) = redirect DashboardR
getUpgradeRepositoryR r = go =<< liftAnnex (repoIdRemote r)
  where
	go Nothing = redirect DashboardR
	go (Just rmt) = do
		liftIO fixSshKeyPairIdentitiesOnly
		liftAnnex $ setConfig 
			(remoteConfig (Remote.repo rmt) "ignore")
			(Git.Config.boolConfig False)
		liftAnnex $ void Remote.remoteListRefresh
		liftAssistant updateSyncRemotes
		liftAssistant $ syncRemote rmt
		redirect DashboardR

{- If there is no currently connected remote, display an alert suggesting
 - to set up one. -}
connectionNeeded :: Handler ()
connectionNeeded = whenM noconnection $ do
	urlrender <- getUrlRender
	void $ liftAssistant $ do
		close <- asIO1 removeAlert
		addAlert $ connectionNeededAlert $ AlertButton
			{ buttonLabel = "Connect"
			, buttonUrl = urlrender ConnectionNeededR
			, buttonAction = Just close
			, buttonPrimary = True
			}
  where
	noconnection = S.null . currentlyConnectedRemotes <$> liftAssistant getDaemonStatus

getConnectionNeededR :: Handler Html
getConnectionNeededR = page "Connection needed" (Just Configuration) $ do
	$(widgetFile "configurators/needconnection")

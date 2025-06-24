{- git-annex assistant webapp configurator for editing existing repos
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
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
import qualified Assistant.WebApp.Configurators.IA as IA
import qualified Remote.S3 as S3
import qualified Remote
import qualified Types.Remote as Remote
import Remote.List.Util
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
import Remote.Helper.Encryptable (extractCipher, parseEncryptionConfig)
import Types.Crypto
import Utility.Gpg
import Annex.UUID
import Annex.Perms
import Assistant.Ssh
import Config
import Config.GitConfig
import Config.DynamicConfig
import Types.Group
import Types.ProposedAccepted
import Annex.SpecialRemote.Config

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
	void uuidDescMapLoad

	groups <- lookupGroups uuid
	remoteconfig <- M.lookup uuid <$> remoteConfigMap
	let (repogroup, associateddirectory) = case getStandardGroup groups of
		Nothing -> (RepoGroupCustom $ unwords $ map fromGroup $ S.toList groups, Nothing)
		Just g -> (RepoGroupStandard g, associatedDirectory remoteconfig g)
	
	description <- fmap (T.pack . fromUUIDDesc) . M.lookup uuid <$> uuidDescMap

	syncable <- case mremote of
		Just r -> liftIO $ getDynamicConfig $ remoteAnnexSync $ Remote.gitconfig r
		Nothing -> getGitConfigVal annexAutoCommit

	return $ RepoConfig
		(T.pack $ maybe "here" Remote.name mremote)
		description
		repogroup
		(T.pack <$> associateddirectory)
		syncable
		
setRepoConfig :: UUID -> Maybe Remote -> RepoConfig -> RepoConfig -> Handler ()
setRepoConfig uuid mremote oldc newc = do
	when descriptionChanged $ liftAnnex $ do
		maybe noop (describeUUID uuid . toUUIDDesc . T.unpack) (repoDescription newc)
		void uuidDescMapLoad
	when nameChanged $ do
		liftAnnex $ do
			name <- uniqueRemoteName (legalName newc) 0 <$> Annex.getGitRemotes
			{- git remote rename expects there to be a
			 - remote.<name>.fetch, and exits nonzero if
			 - there's not. Special remotes don't normally
			 - have that, and don't use it. Temporarily add
			 - it if it's missing. -}
			let remotefetch = Git.ConfigKey $ encodeBS $
				"remote." ++ T.unpack (repoName oldc) ++ ".fetch"
			needfetch <- isNothing <$> fromRepo (Git.Config.getMaybe remotefetch)
			when needfetch $
				inRepo $ Git.Command.run
					[Param "config", Param (Git.fromConfigKey remotefetch), Param ""]
			inRepo $ Git.Command.run
				[ Param "remote"
				, Param "rename"
				, Param $ T.unpack $ repoName oldc
				, Param name
				]
			remotesChanged
		liftAssistant updateSyncRemotes
	when associatedDirectoryChanged $ case repoAssociatedDirectory newc of
		Nothing -> noop
		Just t
			| T.null t -> noop
			| otherwise -> liftAnnex $ do
				let dir = fromOsPath $ takeBaseName $ toOsPath $ T.unpack t
				m <- remoteConfigMap
				case M.lookup uuid m of
					Nothing -> noop
					Just remoteconfig -> configSet uuid $
						M.insert (Proposed "preferreddir") (Proposed dir) remoteconfig
	when groupChanged $ do
		liftAnnex $ case repoGroup newc of
			RepoGroupStandard g -> setStandardGroup uuid g
			RepoGroupCustom s -> groupSet uuid $ S.fromList $ map toGroup $ words s
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

editRepositoryAForm :: Maybe Git.Repo -> Maybe Remote -> RepoConfig -> MkAForm RepoConfig
editRepositoryAForm mrepo mremote d = RepoConfig
	<$> areq (if ishere then readonlyTextField else textField)
		(bfs "Name") (Just $ repoName d)
	<*> aopt textField (bfs "Description") (Just $ repoDescription d)
	<*> areq (selectFieldList groups `withNote` help) (bfs "Repository group") (Just $ repoGroup d)
	<*> associateddirectory
	<*> areq checkBoxField "Syncing enabled" (Just $ repoSyncable d)
  where
	ishere = isNothing mremote
	isspecial = maybe False ((== Git.Unknown) . Git.location) mrepo
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
				giveup "unknown remote"
		curr <- liftAnnex $ getRepoConfig uuid mremote
		liftAnnex $ checkAssociatedDirectory curr mremote
		mrepo <- liftAnnex $
			maybe (pure Nothing) (Just <$$> Remote.getRepo) mremote
		((result, form), enctype) <- liftH $
			runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $
				editRepositoryAForm mrepo mremote curr
		case result of
			FormSuccess input -> liftH $ do
				setRepoConfig uuid mremote curr input
				liftAnnex $ checkAssociatedDirectory input mremote
				redirect DashboardR
			_ -> do
				let istransfer = repoGroup curr == RepoGroupStandard TransferGroup
				config <- liftAnnex $ fromMaybe mempty 
					. M.lookup uuid
					<$> remoteConfigMap
				let repoInfo = getRepoInfo mremote config
				let repoEncryption = getRepoEncryption mremote (Just config)
				$(widgetFile "configurators/edit/repository")
editForm _new r@(RepoName _) = page "Edit repository" (Just Configuration) $ do
	mr <- liftAnnex (repoIdRemote r)
	let repoInfo = case mr of
		Just rmt -> do
			config <- liftAnnex $ fromMaybe mempty
				. M.lookup (Remote.uuid rmt)
				<$> remoteConfigMap
			getRepoInfo mr config
		Nothing -> getRepoInfo Nothing mempty
	g <- liftAnnex gitRepo
	mrepo <- liftAnnex $ maybe (pure Nothing) (Just <$$> Remote.getRepo) mr
	let sshrepo = maybe False (\repo -> remoteLocationIsSshUrl (parseRemoteLocation (Git.repoLocation repo) False g)) mrepo
	$(widgetFile "configurators/edit/nonannexremote")

{- Makes any directory associated with the repository. -}
checkAssociatedDirectory :: RepoConfig -> Maybe Remote -> Annex ()
checkAssociatedDirectory _ Nothing = noop
checkAssociatedDirectory cfg (Just r) = do
	repoconfig <- M.lookup (Remote.uuid r) <$> remoteConfigMap
	case repoGroup cfg of
		RepoGroupStandard gr -> case associatedDirectory repoconfig gr of
			Just d -> do
				top <- fromRepo Git.repoPath
				createWorkTreeDirectory (top </> toOsPath d)
			Nothing -> noop
		_ -> noop

getRepoInfo :: Maybe Remote.Remote -> Remote.RemoteConfig -> Widget
getRepoInfo (Just r) c = case fromProposedAccepted <$> M.lookup typeField c of
	Just "S3" -> do
		pc <- liftAnnex $ parsedRemoteConfig S3.remote c
		if S3.configIA pc
			then IA.getRepoInfo c
			else AWS.getRepoInfo c
	Just t
		| t /= "git" -> [whamlet|#{t} remote|]
	_ -> getGitRepoInfo =<< liftAnnex (Remote.getRepo r)
getRepoInfo _ _ = [whamlet|git repository|]

getGitRepoInfo :: Git.Repo -> Widget
getGitRepoInfo r = do
	let loc = Git.repoLocation r
	[whamlet|git repository located at <tt>#{loc}</tt>|]

getRepoEncryption :: Maybe Remote.Remote -> Maybe Remote.RemoteConfig -> Widget
getRepoEncryption (Just _) (Just c) = case extractCipher pc of
	Nothing ->
		[whamlet|not encrypted|]
	(Just (SharedCipher _)) ->
		[whamlet|encrypted: encryption key stored in git repository|]
	(Just (EncryptedCipher _ _ ks)) -> desckeys ks
	(Just (SharedPubKeyCipher _ ks)) -> desckeys ks
  where
	pc = either (const (Remote.ParsedRemoteConfig mempty mempty)) id $
		parseEncryptionConfig c
	desckeys (KeyIds { keyIds = ks }) = do
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

getConvertRepositoryR  :: RepoId -> Handler ()
getConvertRepositoryR (RepoUUID _) = redirect DashboardR
getConvertRepositoryR r = go =<< liftAnnex (repoIdRemote r)
  where
	go Nothing = redirect DashboardR
	go (Just rmt) = do
		liftIO fixSshKeyPairIdentitiesOnly
		liftAnnex $ do
			repo <- Remote.getRepo rmt
			setConfig
				(remoteAnnexConfig repo "ignore")
				(Git.Config.boolConfig False)
		liftAnnex remotesChanged
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

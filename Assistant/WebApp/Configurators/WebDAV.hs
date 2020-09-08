{- git-annex assistant webapp configurators for WebDAV remotes
 -
 - Copyright 2012, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Configurators.WebDAV where

import Assistant.WebApp.Common
import Creds
import qualified Remote.WebDAV as WebDAV
import Assistant.WebApp.MakeRemote
import qualified Remote
import Types.Remote (RemoteConfig, config)
import Types.StandardGroups
import Logs.Remote
import Git.Types (RemoteName)
import Assistant.Gpg
import Types.GitConfig
import Annex.SpecialRemote.Config
import Types.ProposedAccepted

import qualified Data.Map as M
import qualified Data.Text as T
import Network.URI

webDAVConfigurator :: Widget -> Handler Html
webDAVConfigurator = page "Add a WebDAV repository" (Just Configuration)

data WebDAVInput = WebDAVInput
	{ user :: Text
	, password :: Text
	, embedCreds :: Bool
	, directory :: Text
	, enableEncryption :: EnableEncryption
	}

toCredPair :: WebDAVInput -> CredPair
toCredPair input = (T.unpack $ user input, T.unpack $ password input)

webDAVCredsAForm :: Maybe CredPair -> MkAForm WebDAVInput
webDAVCredsAForm defcreds = WebDAVInput
	<$> areq textField (bfs "Username or Email") (T.pack . fst <$> defcreds)
	<*> areq passwordField (bfs "Password") (T.pack . snd <$> defcreds)
	<*> pure False
	<*> pure T.empty
	<*> pure NoEncryption -- not used!

getEnableWebDAVR :: UUID -> Handler Html
getEnableWebDAVR = postEnableWebDAVR
postEnableWebDAVR :: UUID -> Handler Html
postEnableWebDAVR uuid = do
	m <- liftAnnex readRemoteLog
	let c = fromJust $ M.lookup uuid m
	let name = fromJust $ lookupName c
	let url = fromProposedAccepted $ fromJust $ M.lookup (Accepted "url") c
	mcreds <- liftAnnex $ do
		dummycfg <- liftIO dummyRemoteGitConfig
		pc <- parsedRemoteConfig WebDAV.remote c
		getRemoteCredPairFor "webdav" pc dummycfg (WebDAV.davCreds uuid)
	case mcreds of
		Just creds -> webDAVConfigurator $ liftH $
			makeWebDavRemote enableSpecialRemote name creds M.empty
		Nothing -> webDAVConfigurator $ showform name url
  where
	showform name url = do
		defcreds <- liftAnnex $ 
			maybe (pure Nothing) previouslyUsedWebDAVCreds $
				urlHost url
		((result, form), enctype) <- liftH $
			runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $
				webDAVCredsAForm defcreds
		case result of
			FormSuccess input -> liftH $
				makeWebDavRemote enableSpecialRemote name (toCredPair input) M.empty
			_ -> do
				description <- liftAnnex $
					T.pack <$> Remote.prettyUUID uuid
				$(widgetFile "configurators/enablewebdav")

makeWebDavRemote :: SpecialRemoteMaker -> RemoteName -> CredPair -> RemoteConfig -> Handler ()
makeWebDavRemote maker name creds c = 
	setupCloudRemote TransferGroup Nothing $
		maker name WebDAV.remote (Just creds) c

{- Only returns creds previously used for the same hostname. -}
previouslyUsedWebDAVCreds :: String -> Annex (Maybe CredPair)
previouslyUsedWebDAVCreds hostname =
	previouslyUsedCredPair WebDAV.davCreds WebDAV.remote samehost
  where
	samehost r = case urlHost =<< WebDAV.configUrl (config r) of
		Nothing -> False
		Just h -> h == hostname

urlHost :: String -> Maybe String
urlHost url = uriRegName <$> (uriAuthority =<< parseURI url)

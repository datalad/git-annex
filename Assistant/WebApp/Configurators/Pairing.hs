{- git-annex assistant webapp configurator for pairing
 -
 - Copyright 2012,2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Assistant.WebApp.Configurators.Pairing where

import Assistant.Pairing
import Assistant.WebApp.Common
import Annex.UUID
import Utility.UserInfo
import Utility.Tor
import Utility.Su
import Assistant.WebApp.Pairing
import Assistant.Alert
import qualified Utility.MagicWormhole as Wormhole
import Assistant.MakeRemote
import Assistant.RemoteControl
import Assistant.Sync
import Assistant.WebApp.SideBar
import Command.P2P (unusedPeerRemoteName, PairingResult(..))
import P2P.Address
import Git
import Annex.Path
import Utility.Process.Transcript

import qualified Data.Map as M
import qualified Data.Text as T
import Control.Concurrent.STM hiding (check)

getStartWormholePairFriendR :: Handler Html
getStartWormholePairFriendR = startWormholePairR PairingWithFriend

getStartWormholePairSelfR :: Handler Html
getStartWormholePairSelfR = startWormholePairR PairingWithSelf

startWormholePairR :: PairingWith -> Handler Html
startWormholePairR pairingwith = whenTorInstalled $ whenWormholeInstalled $
	pairPage $ do
		sucommand <- liftIO $ mkSuCommand "git-annex" [Param "enable-tor"]
		$(widgetFile "configurators/pairing/wormhole/start")

getPrepareWormholePairR :: PairingWith -> Handler Html
getPrepareWormholePairR pairingwith = do
	enableTor
	myaddrs <- liftAnnex loadP2PAddresses
	remotename <- liftAnnex unusedPeerRemoteName
	h <- liftAssistant $
		startWormholePairing pairingwith remotename myaddrs
	i <- liftIO . addWormholePairingState h
		=<< wormholePairingState <$> getYesod
	redirect $ RunningWormholePairR i

enableTor :: Handler ()
enableTor = do
	gitannex <- fromOsPath <$> liftIO programPath
	(transcript, ok) <- liftIO $ processTranscript gitannex ["enable-tor"] Nothing
	if ok
		-- Reload remotedameon so it's serving the tor hidden
		-- service.
		then liftAssistant $ sendRemoteControl RELOAD
		else giveup $ "Failed to enable tor\n\n" ++ transcript

getRunningWormholePairR :: WormholePairingId -> Handler Html
getRunningWormholePairR = runningWormholePairR

postRunningWormholePairR :: WormholePairingId -> Handler Html
postRunningWormholePairR = runningWormholePairR

runningWormholePairR :: WormholePairingId -> Handler Html
runningWormholePairR i = go =<< getWormholePairingHandle i
  where
	go Nothing = redirect StartWormholePairFriendR
	go (Just h) = pairPage $  withPairingWith h $ \pairingwith -> do
		ourcode <- liftIO $ getOurWormholeCode h
		let codeprompt = case pairingwith of
			PairingWithFriend -> "Your friend's pairing code"
			PairingWithSelf -> "The other device's pairing code"
		((result, form), enctype) <- liftH $
			runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $
				areq (checkwormholecode ourcode pairingwith textField) (bfs codeprompt) Nothing
		case result of
			FormSuccess t -> case Wormhole.toCode (T.unpack t) of
				Nothing -> giveup invalidcode
				Just theircode -> finish h theircode
			_ -> showform form enctype ourcode pairingwith
	
	showform form enctype ourcode pairingwith =
		$(widgetFile "configurators/pairing/wormhole/prompt")
	
	checkwormholecode ourcode pairingwith = check $ \t ->
		case Wormhole.toCode (T.unpack t) of
			Nothing -> Left (T.pack invalidcode)
			Just theircode
				| theircode == ourcode -> Left $
					case pairingwith of
						PairingWithSelf -> "Oops -- You entered this repository's pairing code. Enter the pairing code of the *other* repository."
						PairingWithFriend -> "Oops -- You entered your pairing code. Enter your friend's pairing code."
				| otherwise -> Right t

	invalidcode = "That does not look like a valid pairing code. Try again..."

	finish h theircode = do
		void $ liftIO $ sendTheirWormholeCode h theircode
		res <- liftAssistant $ finishWormholePairing h
		case res of
			SendFailed -> giveup "Failed sending data to pair."
			ReceiveFailed -> giveup "Failed receiving data from pair."
			LinkFailed e -> giveup $ "Failed linking to pair: " ++ e
			PairSuccess -> withRemoteName h $ \remotename -> do
				r <- liftAnnex $ addRemote (return remotename)
				liftAssistant $ syncRemote r
				liftAssistant $ sendRemoteControl RELOAD
				redirect DashboardR

getWormholePairingHandle :: WormholePairingId -> Handler (Maybe WormholePairingHandle)
getWormholePairingHandle i = do
	s <- wormholePairingState <$> getYesod
	liftIO $ atomically $ M.lookup i <$> readTVar s

whenTorInstalled :: Handler Html -> Handler Html
whenTorInstalled a = ifM (liftIO torIsInstalled)
	( a
	, page "Need Tor" (Just Configuration) $
		$(widgetFile "configurators/needtor")
	)

whenWormholeInstalled :: Handler Html -> Handler Html
whenWormholeInstalled a = ifM (liftIO Wormhole.isInstalled)
	( a
	, page "Need Magic Wormhole" (Just Configuration) $
		$(widgetFile "configurators/needmagicwormhole")
	)

pairPage :: Widget -> Handler Html
pairPage = page "Pairing" (Just Configuration)

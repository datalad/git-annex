{- git-annex assistant alerts
 -
 - Copyright 2012, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, CPP #-}

module Assistant.Alert where

import Common.Annex
import Assistant.Types.Alert
import Assistant.Alert.Utility
import qualified Remote
import Utility.Tense
import Logs.Transfer

import Data.String
import qualified Data.Text as T

#ifdef WITH_WEBAPP
import Assistant.Monad
import Assistant.DaemonStatus
import Assistant.WebApp.Types
import Assistant.WebApp
import Yesod
#endif

{- Makes a button for an alert that opens a Route. The button will
 - close the alert it's attached to when clicked. -}
#ifdef WITH_WEBAPP
mkAlertButton :: T.Text -> UrlRenderer -> Route WebApp -> Assistant AlertButton
mkAlertButton label urlrenderer route = do
	close <- asIO1 removeAlert
	url <- liftIO $ renderUrl urlrenderer route []
	return $ AlertButton
		{ buttonLabel = label
		, buttonUrl = url
		, buttonAction = Just close
		}
#endif

baseActivityAlert :: Alert
baseActivityAlert = Alert
	{ alertClass = Activity
	, alertHeader = Nothing
	, alertMessageRender = tenseWords
	, alertData = []
	, alertBlockDisplay = False
	, alertClosable = False
	, alertPriority = Medium
	, alertIcon = Just ActivityIcon
	, alertCombiner = Nothing
	, alertName = Nothing
	, alertButton = Nothing
	}

warningAlert :: String -> String -> Alert
warningAlert name msg = Alert
	{ alertClass = Warning
	, alertHeader = Just $ tenseWords ["warning"]
	, alertMessageRender = tenseWords
	, alertData = [UnTensed $ T.pack msg]
	, alertBlockDisplay = True
	, alertClosable = True
	, alertPriority = High
	, alertIcon = Just ErrorIcon
	, alertCombiner = Just $ dataCombiner (++)
	, alertName = Just $ WarningAlert name
	, alertButton = Nothing
	}

activityAlert :: Maybe TenseText -> [TenseChunk] -> Alert
activityAlert header dat = baseActivityAlert
	{ alertHeader = header
	, alertData = dat
	}

startupScanAlert :: Alert
startupScanAlert = activityAlert Nothing
	[Tensed "Performing" "Performed", "startup scan"]

commitAlert :: Alert
commitAlert = activityAlert Nothing
	[Tensed "Committing" "Committed", "changes to git"]

showRemotes :: [Remote] -> TenseChunk
showRemotes = UnTensed . T.intercalate ", " . map (T.pack . Remote.name)

syncAlert :: [Remote] -> Alert
syncAlert rs = baseActivityAlert
	{ alertName = Just SyncAlert
	, alertHeader = Just $ tenseWords
		[Tensed "Syncing" "Synced", "with", showRemotes rs]
	, alertPriority = Low
	, alertIcon = Just SyncIcon
	}

syncResultAlert :: [Remote] -> [Remote] -> Alert
syncResultAlert succeeded failed = makeAlertFiller (not $ null succeeded) $
	baseActivityAlert
		{ alertName = Just SyncAlert
		, alertHeader = Just $ tenseWords msg
		}
  where
  	msg
		| null succeeded = ["Failed to sync with", showRemotes failed]
		| null failed = ["Synced with", showRemotes succeeded]
		| otherwise =
			[ "Synced with", showRemotes succeeded
			, "but not with", showRemotes failed
			]

sanityCheckAlert :: Alert
sanityCheckAlert = activityAlert
	(Just $ tenseWords [Tensed "Running" "Ran", "daily sanity check"])
	["to make sure everything is ok."]

sanityCheckFixAlert :: String -> Alert
sanityCheckFixAlert msg = Alert
	{ alertClass = Warning
	, alertHeader = Just $ tenseWords ["Fixed a problem"]
	, alertMessageRender = render
	, alertData = [UnTensed $ T.pack msg]
	, alertBlockDisplay = True
	, alertPriority = High
	, alertClosable = True
	, alertIcon = Just ErrorIcon
	, alertName = Just SanityCheckFixAlert
	, alertCombiner = Just $ dataCombiner (++)
	, alertButton = Nothing
	}
  where
	render dta = tenseWords $ alerthead : dta ++ [alertfoot]
	alerthead = "The daily sanity check found and fixed a problem:"
	alertfoot = "If these problems persist, consider filing a bug report."

pairingAlert :: AlertButton -> Alert
pairingAlert button = baseActivityAlert
	{ alertData = [ UnTensed "Pairing in progress" ]
	, alertPriority = High
	, alertButton = Just button
	}

pairRequestReceivedAlert :: String -> AlertButton -> Alert
pairRequestReceivedAlert who button = Alert
	{ alertClass = Message
	, alertHeader = Nothing
	, alertMessageRender = tenseWords
	, alertData = [UnTensed $ T.pack $ who ++ " is sending a pair request."]
	, alertBlockDisplay = False
	, alertPriority = High
	, alertClosable = True
	, alertIcon = Just InfoIcon
	, alertName = Just $ PairAlert who
	, alertCombiner = Just $ dataCombiner $ \_old new -> new
	, alertButton = Just button
	}

pairRequestAcknowledgedAlert :: String -> Maybe AlertButton -> Alert
pairRequestAcknowledgedAlert who button = baseActivityAlert
	{ alertData = ["Pairing with", UnTensed (T.pack who), Tensed "in progress" "complete"]
	, alertPriority = High
	, alertName = Just $ PairAlert who
	, alertCombiner = Just $ dataCombiner $ \_old new -> new
	, alertButton = button
	}

xmppNeededAlert :: AlertButton -> Alert
xmppNeededAlert button = Alert
	{ alertHeader = Just "Share with friends, and keep your devices in sync across the cloud."
	, alertIcon = Just TheCloud
	, alertPriority = High
	, alertButton = Just button
	, alertClosable = True
	, alertClass = Message
	, alertMessageRender = tenseWords
	, alertBlockDisplay = True
	, alertName = Just $ XMPPNeededAlert
	, alertCombiner = Just $ dataCombiner $ \_old new -> new
	, alertData = []
	}

cloudRepoNeededAlert :: Maybe String -> AlertButton -> Alert
cloudRepoNeededAlert friendname button = Alert
	{ alertHeader = Just $ fromString $ unwords
		[ "Unable to download files from"
		, (fromMaybe "your other devices" friendname) ++ "."
		]
	, alertIcon = Just ErrorIcon
	, alertPriority = High
	, alertButton = Just button
	, alertClosable = True
	, alertClass = Message
	, alertMessageRender = tenseWords
	, alertBlockDisplay = True
	, alertName = Just $ CloudRepoNeededAlert
	, alertCombiner = Just $ dataCombiner $ \_old new -> new
	, alertData = []
	}

remoteRemovalAlert :: String -> AlertButton -> Alert
remoteRemovalAlert desc button = Alert
	{ alertHeader = Just $ fromString $
		"The repository \"" ++ desc ++ 
		"\" has been emptied, and can now be removed."
	, alertIcon = Just InfoIcon
	, alertPriority = High
	, alertButton = Just button
	, alertClosable = True
	, alertClass = Message
	, alertMessageRender = tenseWords
	, alertBlockDisplay = True
	, alertName = Just $ RemoteRemovalAlert desc
	, alertCombiner = Just $ dataCombiner $ \_old new -> new
	, alertData = []
	}

fileAlert :: TenseChunk -> FilePath -> Alert
fileAlert msg file = (activityAlert Nothing [f])
	{ alertName = Just $ FileAlert msg
	, alertMessageRender = render
	, alertCombiner = Just $ dataCombiner combiner
	}
  where
	f = fromString $ shortFile $ takeFileName file
	render fs = tenseWords $ msg : fs
	combiner new old = take 10 $ new ++ old

addFileAlert :: FilePath -> Alert
addFileAlert = fileAlert (Tensed "Adding" "Added")

{- This is only used as a success alert after a transfer, not during it. -}
transferFileAlert :: Direction -> Bool -> FilePath -> Alert
transferFileAlert direction True
	| direction == Upload = fileAlert "Uploaded"
	| otherwise = fileAlert "Downloaded"
transferFileAlert direction False
	| direction == Upload = fileAlert "Upload failed"
	| otherwise = fileAlert "Download failed"

dataCombiner :: ([TenseChunk] -> [TenseChunk] -> [TenseChunk]) -> AlertCombiner
dataCombiner combiner new old
	| alertClass new /= alertClass old = Nothing
	| alertName new == alertName old = 
		Just $! old { alertData = alertData new `combiner` alertData old }
	| otherwise = Nothing

shortFile :: FilePath -> String
shortFile f
	| len < maxlen = f
	| otherwise = take half f ++ ".." ++ drop (len - half) f
  where
	len = length f
	maxlen = 20
	half = (maxlen - 2) `div` 2 


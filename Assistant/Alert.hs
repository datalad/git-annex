{- git-annex assistant alerts
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, CPP, BangPatterns #-}

module Assistant.Alert where

import Annex.Common
import Assistant.Types.Alert
import Assistant.Alert.Utility
import qualified Remote
import Utility.Tense
import Types.Transfer
import Types.Distribution
import Git.Types (RemoteName)

import Data.String
import qualified Data.Text as T
import qualified Control.Exception as E

#ifdef WITH_WEBAPP
import Assistant.DaemonStatus
import Assistant.WebApp.Types
import Assistant.WebApp (renderUrl)
#endif
import Assistant.Monad
import Assistant.Types.UrlRenderer

{- Makes a button for an alert that opens a Route. 
 -
 - If autoclose is set, the button will close the alert it's
 - attached to when clicked. -}
#ifdef WITH_WEBAPP
mkAlertButton :: Bool -> T.Text -> UrlRenderer -> Route WebApp -> Assistant AlertButton
mkAlertButton autoclose label urlrenderer route = do
	close <- asIO1 removeAlert
	url <- liftIO $ renderUrl urlrenderer route []
	return $ AlertButton
		{ buttonLabel = label
		, buttonUrl = url
		, buttonAction = if autoclose then Just close else Nothing
		, buttonPrimary = True
		}
#endif

renderData :: Alert -> TenseText
renderData = tenseWords . alertData

baseActivityAlert :: Alert
baseActivityAlert = Alert
	{ alertClass = Activity
	, alertHeader = Nothing
	, alertMessageRender = renderData
	, alertData = []
	, alertCounter = 0
	, alertBlockDisplay = False
	, alertClosable = False
	, alertPriority = Medium
	, alertIcon = Just ActivityIcon
	, alertCombiner = Nothing
	, alertName = Nothing
	, alertButtons = []
	}

warningAlert :: String -> String -> Alert
warningAlert name msg = Alert
	{ alertClass = Warning
	, alertHeader = Just $ tenseWords ["warning"]
	, alertMessageRender = renderData
	, alertData = [UnTensed $ T.pack msg]
	, alertCounter = 0
	, alertBlockDisplay = True
	, alertClosable = True
	, alertPriority = High
	, alertIcon = Just ErrorIcon
	, alertCombiner = Just $ dataCombiner $ \_old new -> new
	, alertName = Just $ WarningAlert name
	, alertButtons = []
	}

errorAlert :: String -> [AlertButton] -> Alert
errorAlert msg buttons = Alert
	{ alertClass = Error
	, alertHeader = Nothing
	, alertMessageRender = renderData
	, alertData = [UnTensed $ T.pack msg]
	, alertCounter = 0
	, alertBlockDisplay = True
	, alertClosable = True
	, alertPriority = Pinned
	, alertIcon = Just ErrorIcon
	, alertCombiner = Nothing
	, alertName = Nothing
	, alertButtons = buttons
	}

activityAlert :: Maybe TenseText -> [TenseChunk] -> Alert
activityAlert header dat = baseActivityAlert
	{ alertHeader = header
	, alertData = dat
	}

startupScanAlert :: Alert
startupScanAlert = activityAlert Nothing
	[Tensed "Performing" "Performed", "startup scan"]

{- Displayed when a shutdown is occurring, so will be seen after shutdown
 - has happened. -}
shutdownAlert :: Alert
shutdownAlert = warningAlert "shutdown" "git-annex has been shut down"

commitAlert :: Alert
commitAlert = activityAlert Nothing
	[Tensed "Committing" "Committed", "changes to git"]

showRemotes :: [RemoteName] -> TenseChunk
showRemotes = UnTensed . T.intercalate ", " . map T.pack

syncAlert :: [Remote] -> Alert
syncAlert = syncAlert' . map Remote.name

syncAlert' :: [RemoteName] -> Alert
syncAlert' rs = baseActivityAlert
	{ alertName = Just SyncAlert
	, alertHeader = Just $ tenseWords
		[Tensed "Syncing" "Synced", "with", showRemotes rs]
	, alertPriority = Low
	, alertIcon = Just SyncIcon
	}

syncResultAlert :: [Remote] -> [Remote] -> Alert
syncResultAlert succeeded failed = syncResultAlert'
	(map Remote.name succeeded)
	(map Remote.name failed)

syncResultAlert' :: [RemoteName] -> [RemoteName] -> Alert
syncResultAlert' succeeded failed = makeAlertFiller (not $ null succeeded) $
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
	, alertCounter = 0
	, alertBlockDisplay = True
	, alertPriority = High
	, alertClosable = True
	, alertIcon = Just ErrorIcon
	, alertName = Just SanityCheckFixAlert
	, alertCombiner = Just $ dataCombiner (++)
	, alertButtons = []
	}
  where
	render alert = tenseWords $ alerthead : alertData alert ++ [alertfoot]
	alerthead = "The daily sanity check found and fixed a problem:"
	alertfoot = "If these problems persist, consider filing a bug report."

fsckingAlert :: AlertButton -> Maybe Remote -> Alert
fsckingAlert button mr = baseActivityAlert
	{ alertData = case mr of
		Nothing -> [ UnTensed $ T.pack $ "Consistency check in progress" ]
		Just r -> [ UnTensed $ T.pack $ "Consistency check of " ++ Remote.name r ++ " in progress"]
	, alertButtons = [button]
	}

showFscking :: UrlRenderer -> Maybe Remote -> IO (Either E.SomeException a) -> Assistant a
showFscking urlrenderer mr a = do
#ifdef WITH_WEBAPP
	button <- mkAlertButton False (T.pack "Configure") urlrenderer ConfigFsckR
	r <- alertDuring (fsckingAlert button mr) $
		liftIO a
#else
	r <- liftIO a
#endif
	either (liftIO . E.throwIO) return r

notFsckedNudge :: UrlRenderer -> Maybe Remote -> Assistant ()
#ifdef WITH_WEBAPP
notFsckedNudge urlrenderer mr = do
	button <- mkAlertButton True (T.pack "Configure") urlrenderer ConfigFsckR
	void $ addAlert (notFsckedAlert mr button)
#else
notFsckedNudge _ _ = noop
#endif

notFsckedAlert :: Maybe Remote -> AlertButton -> Alert
notFsckedAlert mr button = Alert
	{ alertHeader = Just $ fromString $ concat
		[ "You should enable consistency checking to protect your data"
		, maybe "" (\r -> " in " ++ Remote.name r) mr
		, "."
		]
	, alertIcon = Just InfoIcon
	, alertPriority = High
	, alertButtons = [button]
	, alertClosable = True
	, alertClass = Message
	, alertMessageRender = renderData
	, alertCounter = 0
	, alertBlockDisplay = True
	, alertName = Just NotFsckedAlert
	, alertCombiner = Just $ dataCombiner $ \_old new -> new
	, alertData = []
	}

baseUpgradeAlert :: [AlertButton] -> TenseText -> Alert
baseUpgradeAlert buttons message = Alert
	{ alertHeader = Just message
	, alertIcon = Just UpgradeIcon
	, alertPriority = High
	, alertButtons = buttons
	, alertClosable = True
	, alertClass = Message
	, alertMessageRender = renderData
	, alertCounter = 0
	, alertBlockDisplay = True
	, alertName = Just UpgradeAlert
	, alertCombiner = Just $ fullCombiner $ \new _old -> new
	, alertData = []
	}

canUpgradeAlert :: AlertPriority -> GitAnnexVersion -> AlertButton -> Alert
canUpgradeAlert priority version button = 
	(baseUpgradeAlert [button] $ fromString msg)
		{ alertPriority = priority
		, alertData = [fromString $ " (version " ++ version ++ ")"]
		}
  where
	msg = if priority >= High
		then "An important upgrade of git-annex is available!"
		else "An upgrade of git-annex is available."

upgradeReadyAlert :: AlertButton -> Alert
upgradeReadyAlert button = baseUpgradeAlert [button] $
	fromString "A new version of git-annex has been installed."

upgradingAlert :: Alert
upgradingAlert = activityAlert Nothing [ fromString "Upgrading git-annex" ]

upgradeFinishedAlert :: Maybe AlertButton -> GitAnnexVersion -> Alert
upgradeFinishedAlert button version =
	baseUpgradeAlert (maybeToList button) $ fromString $ 
		"Finished upgrading git-annex to version " ++ version

upgradeFailedAlert :: String -> Alert
upgradeFailedAlert msg = (errorAlert msg [])
	{ alertHeader = Just $ fromString "Upgrade failed." }

unusedFilesAlert :: [AlertButton] -> String -> Alert
unusedFilesAlert buttons message = Alert
	{ alertHeader = Just $ fromString $ unwords
		[ "Old and deleted files are piling up --"
		, message
		]
	, alertIcon = Just InfoIcon
	, alertPriority = High
	, alertButtons = buttons
	, alertClosable = True
	, alertClass = Message
	, alertMessageRender = renderData
	, alertCounter = 0
	, alertBlockDisplay = True
	, alertName = Just UnusedFilesAlert
	, alertCombiner = Just $ fullCombiner $ \new _old -> new
	, alertData = []
	}

brokenRepositoryAlert :: [AlertButton] -> Alert
brokenRepositoryAlert = errorAlert "Serious problems have been detected with your repository. This needs your immediate attention!"

repairingAlert :: String -> Alert
repairingAlert repodesc = activityAlert Nothing
	[ Tensed "Attempting to repair" "Repaired"
	, UnTensed $ T.pack repodesc
	]

pairingAlert :: AlertButton -> Alert
pairingAlert button = baseActivityAlert
	{ alertData = [ UnTensed "Pairing in progress" ]
	, alertPriority = High
	, alertButtons = [button]
	}

pairRequestReceivedAlert :: String -> AlertButton -> Alert
pairRequestReceivedAlert who button = Alert
	{ alertClass = Message
	, alertHeader = Nothing
	, alertMessageRender = renderData
	, alertData = [UnTensed $ T.pack $ who ++ " is sending a pair request."]
	, alertCounter = 0
	, alertBlockDisplay = False
	, alertPriority = High
	, alertClosable = True
	, alertIcon = Just InfoIcon
	, alertName = Just $ PairAlert who
	, alertCombiner = Just $ dataCombiner $ \_old new -> new
	, alertButtons = [button]
	}

pairRequestAcknowledgedAlert :: String -> Maybe AlertButton -> Alert
pairRequestAcknowledgedAlert who button = baseActivityAlert
	{ alertData = ["Pairing with", UnTensed (T.pack who), Tensed "in progress" "complete"]
	, alertPriority = High
	, alertName = Just $ PairAlert who
	, alertCombiner = Just $ dataCombiner $ \_old new -> new
	, alertButtons = maybeToList button
	}

connectionNeededAlert :: AlertButton -> Alert
connectionNeededAlert button = Alert
	{ alertHeader = Just "Share with friends, and keep your devices in sync across the cloud."
	, alertIcon = Just ConnectionIcon
	, alertPriority = High
	, alertButtons = [button]
	, alertClosable = True
	, alertClass = Message
	, alertMessageRender = renderData
	, alertCounter = 0
	, alertBlockDisplay = True
	, alertName = Just ConnectionNeededAlert
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
	, alertButtons = [button]
	, alertClosable = True
	, alertClass = Message
	, alertMessageRender = renderData
	, alertCounter = 0
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
	, alertButtons = [button]
	, alertClosable = True
	, alertClass = Message
	, alertMessageRender = renderData
	, alertCounter = 0
	, alertBlockDisplay = True
	, alertName = Just $ RemoteRemovalAlert desc
	, alertCombiner = Just $ dataCombiner $ \_old new -> new
	, alertData = []
	}

{- Show a message that relates to a list of files.
 -
 - The most recent several files are shown, and a count of any others. -}
fileAlert :: TenseChunk -> [FilePath] -> Alert
fileAlert msg files = (activityAlert Nothing shortfiles)
	{ alertName = Just $ FileAlert msg
	, alertMessageRender = renderer
	, alertCounter = counter
	, alertCombiner = Just $ fullCombiner combiner
	}
  where
	maxfilesshown = 10

	(!somefiles, !counter) = splitcounter (dedupadjacent files)
	!shortfiles = map (fromString . shortFile . takeFileName) somefiles

	renderer alert = tenseWords $ msg : alertData alert ++ showcounter
	  where
		showcounter = case alertCounter alert of
			0 -> []
			_ -> [fromString $ "and " ++ show (alertCounter alert) ++ " other files"]

	dedupadjacent (x:y:rest)
		| x == y = dedupadjacent (y:rest)
		| otherwise = x : dedupadjacent (y:rest)
	dedupadjacent (x:[]) = [x]
	dedupadjacent [] = []

	{- Note that this ensures the counter is never 1; no need to say 
	 - "1 file" when the filename could be shown. -}
	splitcounter l
		| length l <= maxfilesshown = (l, 0)
		| otherwise =
			let (keep, rest) = splitAt (maxfilesshown - 1) l
			in (keep, length rest)
	
	combiner new old =
		let (!fs, n) = splitcounter $
			dedupadjacent $ alertData new ++ alertData old
		    !cnt = n + alertCounter new + alertCounter old
		in old
			{ alertData = fs
			, alertCounter = cnt
			}

addFileAlert :: [FilePath] -> Alert
addFileAlert = fileAlert (Tensed "Adding" "Added")

{- This is only used as a success alert after a transfer, not during it. -}
transferFileAlert :: Direction -> Bool -> FilePath -> Alert
transferFileAlert direction True file
	| direction == Upload = fileAlert "Uploaded" [file]
	| otherwise = fileAlert "Downloaded" [file]
transferFileAlert direction False file
	| direction == Upload = fileAlert "Upload failed" [file]
	| otherwise = fileAlert "Download failed" [file]

dataCombiner :: ([TenseChunk] -> [TenseChunk] -> [TenseChunk]) -> AlertCombiner
dataCombiner combiner = fullCombiner $
	\new old -> old { alertData = alertData new `combiner` alertData old }

fullCombiner :: (Alert -> Alert -> Alert) -> AlertCombiner
fullCombiner combiner new old
	| alertClass new /= alertClass old = Nothing
	| alertName new == alertName old = 
		Just $! new `combiner` old
	| otherwise = Nothing

shortFile :: FilePath -> String
shortFile f
	| len < maxlen = f
	| otherwise = take half f ++ ".." ++ drop (len - half) f
  where
	len = length f
	maxlen = 20
	half = (maxlen - 2) `div` 2 


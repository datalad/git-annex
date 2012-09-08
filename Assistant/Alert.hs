{- git-annex assistant alerts
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes, BangPatterns, OverloadedStrings #-}

module Assistant.Alert where

import Common.Annex
import qualified Remote
import Utility.Tense
import Logs.Transfer

import qualified Data.Text as T
import qualified Data.Map as M
import Data.String

{- Different classes of alerts are displayed differently. -}
data AlertClass = Success | Message | Activity | Warning | Error
	deriving (Eq, Ord)

data AlertPriority = Filler | Low | Medium | High | Pinned
	deriving (Eq, Ord)

{- An alert can have an name, which is used to combine it with other similar
 - alerts. -}
data AlertName 
	= FileAlert TenseChunk
	| SanityCheckFixAlert
	| WarningAlert String
	| PairRequestAlert String
	deriving (Eq)

{- The first alert is the new alert, the second is an old alert.
 - Should return a modified version of the old alert. -}
type AlertCombiner = Alert -> Alert -> Maybe Alert

data Alert = Alert
	{ alertClass :: AlertClass
	, alertHeader :: Maybe TenseText
	, alertMessageRender :: [TenseChunk] -> TenseText
	, alertData :: [TenseChunk]
	, alertBlockDisplay :: Bool
	, alertClosable :: Bool
	, alertPriority :: AlertPriority
	, alertIcon :: Maybe String
	, alertCombiner :: Maybe AlertCombiner
	, alertName :: Maybe AlertName
	}

type AlertPair = (AlertId, Alert)

type AlertMap = M.Map AlertId Alert

{- Higher AlertId indicates a more recent alert. -}
newtype AlertId = AlertId Integer
        deriving (Read, Show, Eq, Ord)

firstAlertId :: AlertId
firstAlertId = AlertId 0

nextAlertId :: AlertId -> AlertId
nextAlertId (AlertId i) = AlertId $ succ i

{- This is as many alerts as it makes sense to display at a time.
 - A display might be smaller, or larger, the point is to not overwhelm the
 - user with a ton of alerts. -}
displayAlerts :: Int
displayAlerts = 6

{- This is not a hard maximum, but there's no point in keeping a great
 - many filler alerts in an AlertMap, so when there's more than this many,
 - they start being pruned, down toward displayAlerts. -}
maxAlerts :: Int
maxAlerts = displayAlerts * 2

{- The desired order is the reverse of:
 -
 - - Pinned alerts
 - - High priority alerts, newest first
 - - Medium priority Activity, newest first (mostly used for Activity)
 - - Low priority alerts, newest first
 - - Filler priorty alerts, newest first
 - - Ties are broken by the AlertClass, with Errors etc coming first.
 -}
compareAlertPairs :: AlertPair -> AlertPair -> Ordering
compareAlertPairs
	(aid, Alert { alertClass = aclass, alertPriority = aprio })
	(bid, Alert { alertClass = bclass, alertPriority = bprio })
	 = compare aprio bprio
		`thenOrd` compare aid bid
			`thenOrd` compare aclass bclass

sortAlertPairs :: [AlertPair] -> [AlertPair]
sortAlertPairs = sortBy compareAlertPairs

{- Renders an alert's header for display, if it has one. -}
renderAlertHeader :: Alert -> Maybe T.Text
renderAlertHeader alert = renderTense (alertTense alert) <$> alertHeader alert

{- Renders an alert's message for display. -}
renderAlertMessage :: Alert -> T.Text
renderAlertMessage alert = renderTense (alertTense alert) $
	(alertMessageRender alert) (alertData alert)

alertTense :: Alert -> Tense
alertTense alert
	| alertClass alert == Activity = Present
	| otherwise = Past

{- Checks if two alerts display the same. -}
effectivelySameAlert :: Alert -> Alert -> Bool
effectivelySameAlert x y = all id 
	[ alertClass x == alertClass y
	, alertHeader x == alertHeader y
	, alertData x == alertData y
	, alertBlockDisplay x == alertBlockDisplay y
	, alertClosable x == alertClosable y
	, alertPriority x == alertPriority y
	]

makeAlertFiller :: Bool -> Alert -> Alert
makeAlertFiller success alert
	| isFiller alert = alert
	| otherwise = alert
		{ alertClass = if c == Activity then c' else c
		, alertPriority = Filler
		, alertClosable = True
		, alertIcon = Just $ if success then "ok" else "exclamation-sign"
		}
	where
		c = alertClass alert
		c'
			| success = Success
			| otherwise = Error

isFiller :: Alert -> Bool
isFiller alert = alertPriority alert == Filler

{- Updates the Alertmap, adding or updating an alert.
 -
 - Any old filler that looks the same as the alert is removed.
 -
 - Or, if the alert has an alertCombiner that combines it with
 - an old alert, the old alert is replaced with the result, and the
 - alert is removed.
 -
 - Old filler alerts are pruned once maxAlerts is reached.
 -}
mergeAlert :: AlertId -> Alert -> AlertMap -> AlertMap
mergeAlert i al m = maybe updatePrune updateCombine (alertCombiner al)
	where
		pruneSame k al' = k == i || not (effectivelySameAlert al al')
		pruneBloat m'
			| bloat > 0 = M.fromList $ pruneold $ M.toList m'
			| otherwise = m'
			where
				bloat = M.size m' - maxAlerts
				pruneold l =
			 		let (f, rest) = partition (\(_, a) -> isFiller a) l
					in drop bloat f ++ rest
		updatePrune = pruneBloat $ M.filterWithKey pruneSame $
			M.insertWith' const i al m
		updateCombine combiner = 
			let combined = M.mapMaybe (combiner al) m
			in if M.null combined
				then updatePrune
				else M.delete i $ M.union combined m

baseActivityAlert :: Alert
baseActivityAlert = Alert
	{ alertClass = Activity
	, alertHeader = Nothing
	, alertMessageRender = tenseWords
	, alertData = []
	, alertBlockDisplay = False
	, alertClosable = False
	, alertPriority = Medium
	, alertIcon = Just "refresh"
	, alertCombiner = Nothing
	, alertName = Nothing
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
	, alertIcon = Just "exclamation-sign"
	, alertCombiner = Just $ dataCombiner (++)
	, alertName = Just $ WarningAlert name
	}

activityAlert :: Maybe TenseText -> [TenseChunk] -> Alert
activityAlert header dat = baseActivityAlert
	{ alertHeader = header
	, alertData = dat
	}

startupScanAlert :: Alert
startupScanAlert = activityAlert Nothing $
	[Tensed "Performing" "Performed", "startup scan"]

commitAlert :: Alert
commitAlert = activityAlert Nothing $
	[Tensed "Committing" "Committed", "changes to git"]

showRemotes :: [Remote] -> TenseChunk
showRemotes = UnTensed . T.unwords . map (T.pack . Remote.name)

pushAlert :: [Remote] -> Alert
pushAlert rs = activityAlert Nothing $
	[Tensed "Syncing" "Synced", "with", showRemotes rs]

pushRetryAlert :: [Remote] -> Alert
pushRetryAlert rs = activityAlert
	(Just $ tenseWords [Tensed "Retrying" "Retried", "sync"])
	(["with", showRemotes rs])

syncAlert :: [Remote] -> Alert
syncAlert rs = baseActivityAlert
	{ alertHeader = Just $ tenseWords
		[Tensed "Syncing" "Synced", "with", showRemotes rs]
	, alertData = []
	, alertPriority = Low
        }

scanAlert :: [Remote] -> Alert
scanAlert rs = baseActivityAlert
	{ alertHeader = Just $ tenseWords
		[Tensed "Scanning" "Scanned", showRemotes rs]
	, alertBlockDisplay = True
	, alertPriority = Low
	}

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
	, alertIcon = Just "exclamation-sign"
	, alertName = Just SanityCheckFixAlert
	, alertCombiner = Just $ dataCombiner (++)
	}
	where
		render dta = tenseWords $ alerthead : dta ++ [alertfoot]
		alerthead = "The daily sanity check found and fixed a problem:"
		alertfoot = "If these problems persist, consider filing a bug report."

pairRequestAlert :: String -> String -> Alert
pairRequestAlert repo msg = Alert
	{ alertClass = Message
	, alertHeader = Just $ tenseWords ["Pair request"]
	, alertMessageRender = tenseWords
	, alertData = [UnTensed $ T.pack msg]
	, alertBlockDisplay = True
	, alertPriority = High
	, alertClosable = True
	, alertIcon = Just "info-sign"
	, alertName = Just $ PairRequestAlert repo
	, alertCombiner = Just $ dataCombiner $ const id
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


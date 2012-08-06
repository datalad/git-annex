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
data AlertName = AddFileAlert | DownloadFailedAlert | SanityCheckFixAlert
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

{- Converts a given alert into filler, manipulating it in the AlertMap.
 -
 - Any old filler that looks the same as the reference alert is removed,
 - or, if the input alert has an alertCombine that combines it with
 - old filler, the old filler is replaced with the result, and the
 - input alert is removed.
 -
 - Old filler alerts are pruned once maxAlerts is reached.
 -}
convertToFiller :: AlertId -> Bool -> AlertMap -> AlertMap
convertToFiller i success m = case M.lookup i m of
	Nothing -> m
 	Just al ->
		let al' = makeAlertFiller success al
		in case alertCombiner al' of
			Nothing -> updatePrune al'
			Just combiner -> updateCombine combiner al'
	where
		pruneSame ref k al = k == i || not (effectivelySameAlert ref al)
		pruneBloat m'
			| bloat > 0 = M.fromList $ pruneold $ M.toList m'
			| otherwise = m'
			where
				bloat = M.size m' - maxAlerts
				pruneold l =
			 		let (f, rest) = partition (\(_, al) -> isFiller al) l
					in drop bloat f ++ rest
		updatePrune al = pruneBloat $ M.filterWithKey (pruneSame al) $
			M.insertWith' const i al m
		updateCombine combiner al = 
			let combined = M.mapMaybe (combiner al) m
			in if M.null combined
				then updatePrune al
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

syncMountAlert :: FilePath -> [Remote] -> Alert
syncMountAlert dir rs = baseActivityAlert
	{ alertHeader = Just $ tenseWords
		[Tensed "Syncing" "Sync", "with", showRemotes rs]
	, alertData = map UnTensed
		["You plugged in"
		, T.pack dir
		, " -- let's get it in sync!"
		]
	, alertBlockDisplay = True
	, alertPriority = Low
        }

scanAlert :: Remote -> Alert
scanAlert r = baseActivityAlert
	{ alertHeader = Just $ tenseWords
		[Tensed "Scanning" "Scanned", showRemotes [r]]
	, alertData =
		[ Tensed "Ensuring" "Ensured"
		, "that"
		, showRemotes [r]
		, Tensed "is" "was"
		, "fully in sync."
		]
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

addFileAlert :: FilePath -> Alert
addFileAlert file = (activityAlert Nothing [f])
	{ alertName = Just AddFileAlert
	, alertMessageRender = render
	, alertCombiner = Just $ dataCombiner combiner
	}
	where
		f = fromString $ shortFile $ takeFileName file
		render fs = tenseWords $ Tensed "Adding" "Added" : fs
		combiner new old = take 10 $ new ++ old

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


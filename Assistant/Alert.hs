{- git-annex assistant alerts
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Assistant.Alert where

import Common.Annex
import qualified Remote

import qualified Data.Map as M
import Yesod

type Widget = forall sub master. GWidget sub master ()

{- Different classes of alerts are displayed differently. -}
data AlertClass = Success | Message | Activity | Warning | Error
	deriving (Eq, Ord)

data AlertPriority = Filler | Low | Medium | High | Pinned
	deriving (Eq, Ord)

{- An alert can be a simple message, or an arbitrary Yesod Widget. -}
data AlertMessage = StringAlert String | WidgetAlert (Alert -> Widget)

data Alert = Alert
	{ alertClass :: AlertClass
	, alertHeader :: Maybe String
	, alertMessage :: AlertMessage
	, alertBlockDisplay :: Bool
	, alertClosable :: Bool
	, alertPriority :: AlertPriority
	}

{- Higher AlertId indicates a more recent alert. -}
newtype AlertId = AlertId Integer
        deriving (Read, Show, Eq, Ord)

{- Note: This first alert id is used for yesod's message. -}
firstAlertId :: AlertId
firstAlertId = AlertId 0

nextAlertId :: AlertId -> AlertId
nextAlertId (AlertId i) = AlertId $ succ i

type AlertPair = (AlertId, Alert)

type AlertMap = M.Map AlertId Alert

{- This is as many alerts as it makes sense to display at a time.
 - A display might be smaller ,or larger, the point is to not overwhelm the
 - user with a ton of alerts. -}
displayAlerts :: Int
displayAlerts = 10

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
	(aid, Alert {alertClass = aclass, alertPriority = aprio})
	(bid, Alert {alertClass = bclass, alertPriority = bprio})
	 = compare aprio bprio
		`thenOrd` compare aid bid
			`thenOrd` compare aclass bclass

sortAlertPairs :: [AlertPair] -> [AlertPair]
sortAlertPairs = sortBy compareAlertPairs

makeAlertFiller :: Bool -> Alert -> Alert
makeAlertFiller success alert
	| isFiller alert = alert
	| otherwise = alert
		{ alertClass = if c == Activity then c' else c
		, alertPriority = Filler
		, alertHeader = finished <$> h
		, alertMessage = massage m
		, alertClosable = True
		}
	where
		h = alertHeader alert
		m = alertMessage alert
		c = alertClass alert
		c'
			| success = Success
			| otherwise = Error

		massage (WidgetAlert w) = WidgetAlert w -- renders old on its own
		massage (StringAlert s) = StringAlert $
			maybe (finished s) (const s) h

		finished s
			| success = s ++ ": Ok"
			| otherwise = s ++ ": Failed"

isFiller :: Alert -> Bool
isFiller alert = alertPriority alert == Filler

{- Converts a given alert into filler, manipulating it in the AlertMap.
 -
 - Old filler alerts are pruned once maxAlerts is reached.
 -}
convertToFiller :: AlertId -> Bool -> AlertMap -> AlertMap
convertToFiller i success m
	| bloat > 0 = M.fromList $ prune $ M.toList m'
	| otherwise = m'
	where
		bloat = M.size m - maxAlerts
		m' = M.adjust (\al -> makeAlertFiller success al) i m
		prune l =
			let (f, rest) = partition (\(_, al) -> isFiller al) l
			in drop bloat f ++ rest

baseActivityAlert :: Alert
baseActivityAlert = Alert
	{ alertClass = Activity
	, alertHeader = Nothing
	, alertMessage = StringAlert ""
	, alertBlockDisplay = False
	, alertClosable = False
	, alertPriority = Medium
	}

activityAlert :: Maybe String -> String -> Alert
activityAlert header message = baseActivityAlert
	{ alertHeader = header
	, alertMessage = StringAlert message
	}

startupScanAlert :: Alert
startupScanAlert = activityAlert Nothing "Performing startup scan"

runningAlert :: Alert
runningAlert = baseActivityAlert
	{ alertClass = Success
	, alertMessage = StringAlert "Running"
	, alertPriority = Pinned
	}

pushAlert :: [Remote] -> Alert
pushAlert rs = activityAlert Nothing $
	"Syncing with " ++ unwords (map Remote.name rs)

pushRetryAlert :: [Remote] -> Alert
pushRetryAlert rs = activityAlert (Just "Retrying sync") $
	"with " ++ unwords (map Remote.name rs) ++ ", which failed earlier."

syncMountAlert :: FilePath -> [Remote] -> Alert
syncMountAlert dir rs = baseActivityAlert
	{ alertHeader = Just $ "Syncing with " ++ unwords (map Remote.name rs)
	, alertMessage = StringAlert $ unwords
		["You plugged in"
		, dir
		, " -- let's get it in sync!"
		]
	, alertBlockDisplay = True
	, alertPriority = Low
        }

scanAlert :: Remote -> Alert
scanAlert r = baseActivityAlert
	{ alertHeader = Just $ "Scanning " ++ Remote.name r
	, alertMessage = StringAlert $ unwords
		[ "Ensuring that ", Remote.name r
		, "is fully in sync." ]
	, alertBlockDisplay = True
	, alertPriority = Low
	}

sanityCheckAlert :: Alert
sanityCheckAlert = activityAlert (Just "Running daily sanity check")
	"to make sure everything is ok."

sanityCheckFixAlert :: String -> Alert
sanityCheckFixAlert msg = Alert
	{ alertClass = Warning
	, alertHeader = Just "Fixed a problem"
	, alertMessage = StringAlert $ unwords
		[ "The daily sanity check found and fixed a problem:"
		, msg
		, "If these problems persist, consider filing a bug report."
		]
	, alertBlockDisplay = True
	, alertPriority = High
	, alertClosable = True
	}

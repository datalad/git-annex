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

import Yesod

type Widget = forall sub master. GWidget sub master ()

{- Different classes of alerts are displayed differently. -}
data AlertClass = Success | Message | Activity | Warning | Error
	deriving (Eq, Ord)

{- An alert can be a simple message, or an arbitrary Yesod Widget -}
data AlertMessage = StringAlert String | WidgetAlert Widget

data Alert = Alert
	{ alertClass :: AlertClass
	, alertHeader :: Maybe String
	, alertMessage :: AlertMessage
	, alertBlockDisplay :: Bool
	, alertPriority :: AlertPriority
	}

{- Higher AlertId indicates a more recent alert. -}
type AlertId = Integer

type AlertPair = (AlertId, Alert)

data AlertPriority = Low | Medium | High
	deriving (Eq, Ord)

{- The desired order is the reverse of:
 -
 - - High priority alerts, newest first
 - - Medium priority Activity, newest first (mostly used for Activity)
 - - Low priority alwerts, newest first
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
sortAlertPairs = reverse . sortBy compareAlertPairs

activityAlert :: Maybe String -> String -> Alert
activityAlert header message = Alert
	{ alertClass = Activity
	, alertHeader = header
	, alertMessage = StringAlert message
	, alertBlockDisplay = False
	, alertPriority = Medium
	}

startupScanAlert :: Alert
startupScanAlert = activityAlert Nothing "Performing startup scan"

runningAlert :: Alert
runningAlert = (activityAlert Nothing "Running")
	{ alertClass = Success
	, alertPriority = High -- pin above the other activity alerts
	}

pushAlert :: [Remote] -> Alert
pushAlert rs = activityAlert Nothing $
	"Syncing with " ++ unwords (map Remote.name rs)

pushRetryAlert :: [Remote] -> Alert
pushRetryAlert rs = activityAlert (Just "Retrying sync") $
	"with " ++ unwords (map Remote.name rs) ++ ", which failed earlier."

syncMountAlert :: FilePath -> [Remote] -> Alert
syncMountAlert dir rs = Alert
	{ alertClass = Activity
	, alertHeader = Just $ "Syncing with " ++ unwords (map Remote.name rs)
	, alertMessage = StringAlert $ unwords
		["I noticed you plugged in"
		, dir
		, " -- let's get it in sync!"
		]
	, alertBlockDisplay = True
	, alertPriority = Low
        }

scanAlert :: Remote -> Alert
scanAlert r = Alert
	{ alertClass = Activity
	, alertHeader = Just $ "Scanning " ++ Remote.name r
	, alertMessage = StringAlert $ unwords
		[ "Ensuring that ", Remote.name r
		, "is fully in sync." ]
	, alertBlockDisplay = True
	, alertPriority = Low
	}

sanityCheckAlert :: Alert
sanityCheckAlert = activityAlert (Just "Running daily sanity check")
	"to make sure I've not missed anything."

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
	}

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
data AlertClass = Activity | Warning | Error | Success | Message
	deriving (Eq)

{- An alert can be a simple message, or an arbitrary Yesod Widget -}
data AlertMessage = StringAlert String | WidgetAlert Widget

data Alert = Alert
	{ alertClass :: AlertClass
	, alertHeader :: Maybe String
	, alertMessage :: AlertMessage
	, alertBlockDisplay :: Bool
	}

activityAlert :: Maybe String -> String -> Alert
activityAlert header message = Alert
	{ alertClass = Activity
	, alertHeader = header
	, alertMessage = StringAlert message
	, alertBlockDisplay = False
	}

startupScanAlert :: Alert
startupScanAlert = activityAlert Nothing "Performing startup scan"

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
        }

scanAlert :: Remote -> Alert
scanAlert r = Alert
	{ alertClass = Activity
	, alertHeader = Just $ "Scanning " ++ Remote.name r
	, alertMessage = StringAlert $ unwords
		[ "Ensuring that ", Remote.name r
		, "is fully in sync." ]
	, alertBlockDisplay = True
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
	}

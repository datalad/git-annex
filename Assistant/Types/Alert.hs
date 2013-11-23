{- git-annex assistant alert types
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.Alert where

import Utility.Tense

import Data.Text (Text)
import qualified Data.Map as M

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
	| PairAlert String
	| XMPPNeededAlert
	| RemoteRemovalAlert String
	| CloudRepoNeededAlert
	| SyncAlert
	| NotFsckedAlert
	| CanUpgradeAlert
	| UpgradeReadyAlert
	deriving (Eq)

{- The first alert is the new alert, the second is an old alert.
 - Should return a modified version of the old alert. -}
type AlertCombiner = Alert -> Alert -> Maybe Alert

data Alert = Alert
	{ alertClass :: AlertClass
	, alertHeader :: Maybe TenseText
	, alertMessageRender :: Alert -> TenseText
	, alertData :: [TenseChunk]
	, alertCounter :: Int
	, alertBlockDisplay :: Bool
	, alertClosable :: Bool
	, alertPriority :: AlertPriority
	, alertIcon :: Maybe AlertIcon
	, alertCombiner :: Maybe AlertCombiner
	, alertName :: Maybe AlertName
	, alertButtons :: [AlertButton]
	}

data AlertIcon = ActivityIcon | SyncIcon | SuccessIcon | ErrorIcon | InfoIcon | UpgradeIcon | TheCloud

type AlertMap = M.Map AlertId Alert

{- Higher AlertId indicates a more recent alert. -}
newtype AlertId = AlertId Integer
	deriving (Read, Show, Eq, Ord)

firstAlertId :: AlertId
firstAlertId = AlertId 0

nextAlertId :: AlertId -> AlertId
nextAlertId (AlertId i) = AlertId $ succ i

{- When clicked, a button always redirects to a URL
 - It may also run an IO action in the background, which is useful
 - to make the button close or otherwise change the alert. -}
data AlertButton = AlertButton
	{ buttonLabel :: Text
	, buttonUrl :: Text
	, buttonAction :: Maybe (AlertId -> IO ())
	, buttonPrimary :: Bool
	}

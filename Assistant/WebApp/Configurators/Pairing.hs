{- git-annex assistant webapp configurator for pairing
 -
 - Pairing works like this:
 -
 - * The user optns StartPairR, which prompts them for a secret.
 - * The user submits it. A PairReq is broadcast out. The secret is
 -   stashed away in a list of known pairing secrets.
 - * On another device, it's received, and that causes its webapp to 
 -   display an Alert.
 - * The user there clicks the button, which opens FinishPairR,
 -   which prompts them for the same secret.
 - * The secret is used to verify the PairReq. If it checks out,
 -   a PairAck is sent, and the other device adds the ssh key from the
 -   PairReq. An Alert is displayed noting that the pairing has been set up.
 - * The PairAck is received back at the device that started the process.
 -   It's verified using the stored secret. The ssh key from the PairAck
 -   is added. An Alert is displayed noting that the pairing has been set
 -   up. Note that multiple other devices could also send PairAcks, and
 -   as long as they're valid, all those devices are paired with.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.Pairing where

import Assistant.Common
import Assistant.Pairing
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Utility.Yesod
import Assistant.WebApp.Configurators.Local
import qualified Types.Remote as R
import qualified Remote.Rsync as Rsync
import qualified Command.InitRemote
import Logs.UUID
import Logs.Remote

import Yesod
import Data.Text (Text)
import qualified Data.Text as T

getStartPairR :: Handler RepHtml
getStartPairR = undefined

getFinishPairR :: PairReq -> Handler RepHtml
getFinishPairR = undefined

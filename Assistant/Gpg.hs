{- git-annex assistant gpg stuff
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.Gpg where

import Utility.Gpg
import Utility.UserInfo
import Utility.PartialPrelude
import Types.Remote (RemoteConfigField)
import Annex.SpecialRemote.Config
import Types.ProposedAccepted

import Data.Maybe
import qualified Data.Map as M
import Control.Applicative
import Prelude

{- Generates a gpg user id that is not used by any existing secret key -}
newUserId :: GpgCmd -> IO UserId
newUserId cmd = do
	oldkeys <- secretKeys cmd
	username <- either (const "unknown") id <$> myUserName
	let basekeyname = username ++ "'s git-annex encryption key"
	return $ fromMaybe (error "internal") $ headMaybe $
		filter (\n -> M.null $ M.filter (== n) oldkeys)
			( basekeyname
			: map (\n -> basekeyname ++ show n) ([2..] :: [Int])
			)

data EnableEncryption = HybridEncryption | SharedEncryption | NoEncryption
	deriving (Eq)

{- Generates Remote configuration for encryption. -}
configureEncryption :: EnableEncryption -> (RemoteConfigField, ProposedAccepted String)
configureEncryption SharedEncryption = (encryptionField, Proposed "shared")
configureEncryption NoEncryption = (encryptionField, Proposed "none")
configureEncryption HybridEncryption = (encryptionField, Proposed "hybrid")

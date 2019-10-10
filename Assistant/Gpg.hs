{- git-annex assistant gpg stuff
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.Gpg where

import Utility.Gpg
import Utility.UserInfo
import Types.Remote (RemoteConfigField)
import Annex.SpecialRemote.Config

import qualified Data.Map as M
import Control.Applicative
import Prelude

{- Generates a gpg user id that is not used by any existing secret key -}
newUserId :: GpgCmd -> IO UserId
newUserId cmd = do
	oldkeys <- secretKeys cmd
	username <- either (const "unknown") id <$> myUserName
	let basekeyname = username ++ "'s git-annex encryption key"
	return $ Prelude.head $ filter (\n -> M.null $ M.filter (== n) oldkeys)
		( basekeyname
		: map (\n -> basekeyname ++ show n) ([2..] :: [Int])
		)

data EnableEncryption = HybridEncryption | SharedEncryption | NoEncryption
	deriving (Eq)

{- Generates Remote configuration for encryption. -}
configureEncryption :: EnableEncryption -> (RemoteConfigField, String)
configureEncryption SharedEncryption = (encryptionField, "shared")
configureEncryption NoEncryption = (encryptionField, "none")
configureEncryption HybridEncryption = (encryptionField, "hybrid")

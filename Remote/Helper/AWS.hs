{- Amazon Web Services common infrastructure.
 -
 - Copyright 2011,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.AWS where

import Common.Annex
import Creds

creds :: UUID -> CredPairStorage
creds u = CredPairStorage
	{ credPairFile = fromUUID u
	, credPairEnvironment = ("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")
	, credPairRemoteKey = Just "s3creds"
	}

setCredsEnv :: CredPair -> IO ()
setCredsEnv p = setEnvCredPair p $ creds undefined

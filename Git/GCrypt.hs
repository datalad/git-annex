{- git-remote-gcrypt support
 -
 - https://github.com/blake2-ppc/git-remote-gcrypt
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.GCrypt where

import Common
import Git.Types
import Git.Construct
import qualified Git.Config as Config
import Utility.Gpg

urlPrefix :: String
urlPrefix = "gcrypt::"

isEncrypted :: Repo -> Bool
isEncrypted Repo { location = Url url } = urlPrefix `isPrefixOf` show url
isEncrypted _ = False

{- The first Repo is the git repository that has the second Repo
 - as one of its remotes.
 -
 - When the remote Repo uses gcrypt, returns the actual underlying
 - git repository that gcrypt is using to store its data. 
 -
 - Throws an exception if an url is invalid or the repo does not use
 - gcrypt.
 -}
encryptedRepo :: Repo -> Repo -> IO Repo
encryptedRepo baserepo = go
  where
  	go Repo { location = Url url }
		| urlPrefix `isPrefixOf` u =
			fromRemoteLocation (drop plen u) baserepo
		| otherwise = notencrypted
	  where
  		u = show url
		plen = length urlPrefix
	go _ = notencrypted
	notencrypted = error "not a gcrypt encrypted repository"

type RemoteName = String

{- gcrypt gives each encrypted repository a uique gcrypt-id,
 - which is stored in the repository (in encrypted form)
 - and cached in a per-remote gcrypt-id configuration setting. -}
remoteRepoId :: Repo -> Maybe RemoteName -> Maybe String
remoteRepoId = getRemoteConfig "gcrypt-id"

getRemoteConfig :: String -> Repo -> Maybe RemoteName -> Maybe String
getRemoteConfig field repo remotename = do
	n <- remotename
	Config.getMaybe (remoteConfigKey field n) repo

{- Gpg keys that the remote is encrypted for.
 - If empty, gcrypt uses --default-recipient-self -}
getParticiantList :: Maybe Repo -> Repo -> Maybe RemoteName -> KeyIds
getParticiantList globalconfigrepo repo remotename = KeyIds $ parse $ firstJust
	[ getRemoteConfig "gcrypt-participants" repo remotename
	, Config.getMaybe defaultkey repo
	, Config.getMaybe defaultkey =<< globalconfigrepo
	]
  where
	defaultkey = "gcrypt.participants"
  	parse (Just "simple") = []
	parse (Just l) = words l
	parse Nothing = []

remoteParticipantConfigKey :: RemoteName -> String
remoteParticipantConfigKey = remoteConfigKey "gcrypt-participants"

remoteSigningKey :: RemoteName -> String
remoteSigningKey = remoteConfigKey "gcrypt-signingkey"

remoteConfigKey :: String -> RemoteName -> String
remoteConfigKey key remotename = "remote." ++ remotename ++ "." ++ key

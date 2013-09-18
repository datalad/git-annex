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
import qualified Git.Command as Command
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

{- Checks if the git repo at a location is a gcrypt repo that
 - we can decrypt. This works by trying to fetch from the repo
 - at the location, into the baserepo.
 -
 - Returns false if the git repo is not using gcrypt, or if it is using
 - gcrypt but cannot be decrypted. We do not try to detect gcrypt
 - repos that cannot be decrypted, because gcrypt may change in the future
 - to avoid easy fingerprinting of gcrypt repos.
 -}
probeGCryptRepo :: FilePath -> Repo -> IO Bool
probeGCryptRepo dir baserepo = catchBoolIO $ Command.runBool
	[ Param "fetch"
	, Param $ urlPrefix ++ dir
	] baserepo

type RemoteName = String
type GCryptId = String

{- gcrypt gives each encrypted repository a uique gcrypt-id,
 - which is stored in the repository (in encrypted form)
 - and cached in a per-remote gcrypt-id configuration setting. -}
remoteRepoId :: Repo -> Maybe RemoteName -> Maybe GCryptId
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

{- git-remote-gcrypt support
 -
 - https://spwhitton.name/tech/code/git-remote-gcrypt/
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
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

urlScheme :: String
urlScheme = "gcrypt:"

urlPrefix :: String
urlPrefix = urlScheme ++ ":"

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
encryptedRemote :: Repo -> Repo -> IO Repo
encryptedRemote baserepo = go
  where
	go Repo { location = Url url }
		| urlPrefix `isPrefixOf` u =
			fromRemoteLocation (drop plen u) baserepo
		| otherwise = notencrypted
	  where
		u = show url
		plen = length urlPrefix
	go _ = notencrypted
	notencrypted = giveup "not a gcrypt encrypted repository"

data ProbeResult = Decryptable | NotDecryptable | NotEncrypted

{- Checks if the git repo at a location uses gcrypt.
 - 
 - Rather expensive -- many need to fetch the entire repo contents.
 - (Which is fine if the repo is going to be added as a remote..)
 -}
probeRepo :: String -> Repo -> IO ProbeResult
probeRepo loc baserepo = do
	let p = proc "git" $ toCommand $ Command.gitCommandLine
		[ Param "remote-gcrypt"
		, Param "--check"
		, Param loc
		] baserepo
	(_, _, _, pid) <- createProcess p
	code <- waitForProcess pid
	return $ case code of
		ExitSuccess -> Decryptable
		ExitFailure 1 -> NotDecryptable
		ExitFailure _ -> NotEncrypted

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

remotePublishParticipantConfigKey :: RemoteName -> String
remotePublishParticipantConfigKey = remoteConfigKey "gcrypt-publish-participants"

remoteSigningKey :: RemoteName -> String
remoteSigningKey = remoteConfigKey "gcrypt-signingkey"

remoteConfigKey :: String -> RemoteName -> String
remoteConfigKey key remotename = "remote." ++ remotename ++ "." ++ key

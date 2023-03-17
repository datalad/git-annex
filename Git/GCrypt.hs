{- git-remote-gcrypt support
 -
 - https://spwhitton.name/tech/code/git-remote-gcrypt/
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.GCrypt where

import Common
import Git.Types
import Git.Construct
import qualified Git.Config as Config
import qualified Git.Command as Command
import Utility.Gpg

import qualified Data.ByteString as S
import qualified Network.URI

urlScheme :: String
urlScheme = "gcrypt:"

urlPrefix :: String
urlPrefix = urlScheme ++ ":"

isEncrypted :: Repo -> Bool
isEncrypted Repo { location = Url url } = urlPrefix `isPrefixOf` show url
isEncrypted Repo { location = UnparseableUrl url } = urlPrefix `isPrefixOf` url
isEncrypted _ = False

{- The first Repo is the git repository that has the second Repo
 - as one of its remotes.
 -
 - When the remote Repo uses gcrypt, returns the actual underlying
 - git repository that gcrypt is using to store its data. 
 -
 - Throws an exception if the repo does not use gcrypt.
 -}
encryptedRemote :: Repo -> Repo -> IO Repo
encryptedRemote baserepo = go
  where
	go Repo { location = Url url } = go' (show url)
	go Repo { location = UnparseableUrl url } = go' url
	go _ = notencrypted

	go' u
		| urlPrefix `isPrefixOf` u =
			let l = drop plen u
			    -- Git.Construct.fromUrl escapes characters
			    -- that are not allowed in URIs (though git
			    -- allows them); need to de-escape any such
			    -- to get back the path to the repository.
			    l' = Network.URI.unEscapeString l
			in fromRemoteLocation l' baserepo
		| otherwise = notencrypted

	notencrypted = giveup "not a gcrypt encrypted repository"

	plen = length urlPrefix

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
	withCreateProcess p $ \_ _ _ pid -> do
		code <- waitForProcess pid
		return $ case code of
			ExitSuccess -> Decryptable
			ExitFailure 1 -> NotDecryptable
			ExitFailure _ -> NotEncrypted

type GCryptId = String

{- gcrypt gives each encrypted repository a unique gcrypt-id,
 - which is stored in the repository (in encrypted form)
 - and cached in a per-remote gcrypt-id configuration setting. -}
remoteRepoId :: Repo -> Maybe RemoteName -> Maybe GCryptId
remoteRepoId r n = fromConfigValue <$> getRemoteConfig "gcrypt-id" r n

getRemoteConfig :: S.ByteString -> Repo -> Maybe RemoteName -> Maybe ConfigValue
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
	parse (Just (ConfigValue "simple")) = []
	parse (Just (ConfigValue b)) = words (decodeBS b)
	parse (Just NoConfigValue) = []
	parse Nothing = []

remoteParticipantConfigKey :: RemoteName -> ConfigKey
remoteParticipantConfigKey = remoteConfigKey "gcrypt-participants"

remotePublishParticipantConfigKey :: RemoteName -> ConfigKey
remotePublishParticipantConfigKey = remoteConfigKey "gcrypt-publish-participants"

remoteSigningKey :: RemoteName -> ConfigKey
remoteSigningKey = remoteConfigKey "gcrypt-signingkey"

remoteConfigKey :: S.ByteString -> RemoteName -> ConfigKey
remoteConfigKey key remotename = ConfigKey $
	"remote." <> encodeBS remotename <> "." <> key

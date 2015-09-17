{- git-annex webapp gpg stuff
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Gpg where

import Assistant.WebApp.Common
import Assistant.Gpg
import Utility.Gpg
import qualified Annex
import qualified Git.Command
import qualified Git.Remote.Remove
import qualified Git.Construct
import qualified Annex.Branch
import qualified Git.GCrypt
import qualified Remote.GCrypt as GCrypt
import Git.Types (RemoteName)
import Assistant.WebApp.MakeRemote
import Logs.Remote

import qualified Data.Map as M

gpgKeyDisplay :: KeyId -> Maybe UserId -> Widget
gpgKeyDisplay keyid userid = [whamlet|
  <span title="key id #{keyid}">
    <span .glyphicon .glyphicon-user>
    \ 
    $maybe name <- userid
      #{name}
    $nothing
      key id #{keyid}
|]

genKeyModal :: Widget
genKeyModal = $(widgetFile "configurators/genkeymodal")

isGcryptInstalled :: IO Bool
isGcryptInstalled = inPath "git-remote-gcrypt"

whenGcryptInstalled :: Handler Html -> Handler Html
whenGcryptInstalled a = ifM (liftIO isGcryptInstalled)
	( a
	, page "Need git-remote-gcrypt" (Just Configuration) $
		$(widgetFile "configurators/needgcrypt")
	)

withNewSecretKey :: (KeyId -> Handler Html) -> Handler Html
withNewSecretKey use = do
	cmd <- liftAnnex $ gpgCmd <$> Annex.getGitConfig
	userid <- liftIO $ newUserId cmd
	liftIO $ genSecretKey cmd RSA "" userid maxRecommendedKeySize
	results <- M.keys . M.filter (== userid) <$> liftIO (secretKeys cmd)
	case results of
		[] -> error "Failed to generate gpg key!"
		(key:_) -> use key

{- Tries to find the name used in remote.log for a gcrypt repository
 - with a given uuid.
 -
 - The gcrypt remote may not be on that is listed in the local remote.log
 - (or the info may be out of date), so this actually fetches the git-annex
 - branch from the gcrypt remote and merges it in, and then looks up
 - the name.
 -}
getGCryptRemoteName :: UUID -> String -> Annex RemoteName
getGCryptRemoteName u repoloc = do
	tmpremote <- uniqueRemoteName "tmpgcryptremote" 0 <$> gitRepo
	void $ inRepo $ Git.Command.runBool
		[ Param "remote"
		, Param "add"
		, Param tmpremote
		, Param $ Git.GCrypt.urlPrefix ++ repoloc
		]
	mname <- ifM (inRepo $ Git.Command.runBool [Param "fetch", Param tmpremote])
		( do
			void Annex.Branch.forceUpdate
			(M.lookup "name" <=< M.lookup u) <$> readRemoteLog
		, return Nothing
		)
	void $ inRepo $ Git.Remote.Remove.remove tmpremote
	maybe missing return mname
  where
	missing = error $ "Cannot find configuration for the gcrypt remote at " ++ repoloc

{- Checks to see if a repo is encrypted with gcrypt, and runs one action if
 - it's not an another if it is.
 -
 - Since the probing requires gcrypt to be installed, a third action must
 - be provided to run if it's not installed.
 -}
checkGCryptRepoEncryption :: (Monad m, LiftAnnex m) => String -> m a -> m a -> m a -> m a
checkGCryptRepoEncryption location notencrypted notinstalled encrypted = 
	ifM (liftAnnex $ liftIO isGcryptInstalled)
		( dispatch =<< liftAnnex (inRepo $ Git.GCrypt.probeRepo location)
		, notinstalled
		)
  where
	dispatch Git.GCrypt.Decryptable = encrypted
	dispatch Git.GCrypt.NotEncrypted = notencrypted
	dispatch Git.GCrypt.NotDecryptable =
		error "This git repository is encrypted with a GnuPG key that you do not have."

{- Gets the UUID of the gcrypt repo at a location, which may not exist.
 - Only works if the gcrypt repo was created as a git-annex remote. -}
probeGCryptRemoteUUID :: String -> Annex (Maybe UUID)
probeGCryptRemoteUUID repolocation = do
	r <- inRepo $ Git.Construct.fromRemoteLocation repolocation
	GCrypt.getGCryptUUID False r

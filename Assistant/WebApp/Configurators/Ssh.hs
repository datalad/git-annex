{- git-annex assistant webapp configurator for ssh-based remotes
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.Ssh where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.SideBar
import Utility.Yesod

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Network.BSD
import System.Posix.User

data SshServer = SshServer
	{ hostname :: Maybe Text
	, username :: Maybe Text
	, directory :: Maybe Text
	}
	deriving Show

sshServerAForm :: Text -> AForm WebApp WebApp SshServer
sshServerAForm localusername = SshServer
	<$> aopt check_hostname "Host name" Nothing
	<*> aopt check_username "User name" (Just $ Just localusername)
	<*> aopt textField "Directory" (Just $ Just $ T.pack gitAnnexAssistantDefaultDir)
	where
		check_hostname = checkM (liftIO . checkdns) textField
		checkdns t = do
			let h = T.unpack t
			r <- catchMaybeIO $ getHostByName h
			return $ case r of
				-- canonicalize input hostname if it had no dot
				Just hostentry
					| '.' `elem` h -> Right t
					| otherwise -> Right $ T.pack $ hostName hostentry
				Nothing -> Left bad_hostname

		check_username = checkBool (all (`notElem` "/:@ \t") . T.unpack)
			bad_username textField
		
		bad_hostname = "cannot resolve host name" :: Text
		bad_username = "bad user name" :: Text

data ServerStatus
	= UntestedServer
	| UnusableServer Text -- reason why it's not usable
	| UsableRsyncServer
	| UsableSshServer

{- Test if we can ssh into the server. If ssh doesn't work, fall back to
 - trying rsync protocol.
 -
 - Before sshing in, if the user doesn't have a ssh key, a special one is
 - generated just for this server, and configured to be used for this
 - server. (If the user does have a ssh key, we assume they know what
 - they're doing, and don't touch their ssh setup.)
 -
 - If we can ssh in, check that git-annex-shell is installed. If not, this
 - will need to be a rsync special remote, rather than a git remote, so
 - check that rsync is installed.
 -
 - When we ssh in, if we set up a ssh key, the server's authorized_keys
 - is configured to let it run either git-annex-shell or rsync for that ssh
 - key, and nothing else.
 -
 - Of course, ssh may ask for a passphrase, etc. We rely on ssh-askpass
 - or an equivilant being used by ssh. Or, if the assistant is
 - running in the foreground, the password will be asked there.
 -}
testServer :: SshServer -> IO ServerStatus
testServer (SshServer { hostname = Nothing }) = return $
	UnusableServer "Please enter a host name."
testServer _sshserver = return UsableSshServer

getAddSshR :: Handler RepHtml
getAddSshR = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Add a remote server"
	u <- liftIO $ T.pack . userName
		<$> (getUserEntryForID =<< getEffectiveUserID)
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap $ sshServerAForm u
	case result of
		FormSuccess sshserver -> do
			showform form enctype =<< liftIO (testServer sshserver)
		_ -> showform form enctype UntestedServer
	where
		showform form enctype status = do
			let authtoken = webAppFormAuthToken
			$(widgetFile "configurators/addssh")

		buttonText :: ServerStatus -> Text
		buttonText UsableRsyncServer = "Make rsync repository"
		buttonText UsableSshServer = "Clone repository to ssh server"
		buttonText _ = "Check this server"

		willTest UntestedServer = True
		willTest (UnusableServer _) = True
		willTest _ = False

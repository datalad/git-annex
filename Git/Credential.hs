{- git credential interface
 -
 - Copyright 2019-2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.Credential where

import Common
import Git
import Git.Types
import Git.Command
import qualified Git.Config as Config
import Utility.Url
import Utility.Url.Parse

import qualified Data.Map as M
import Network.URI
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Control.Concurrent.STM

data Credential = Credential { fromCredential :: M.Map String String }

credentialUsername :: Credential -> Maybe String
credentialUsername = M.lookup "username" . fromCredential

credentialPassword :: Credential -> Maybe String
credentialPassword = M.lookup "password" . fromCredential

credentialBasicAuth :: Credential -> Maybe BasicAuth
credentialBasicAuth cred = BasicAuth
	<$> credentialUsername cred
	<*> credentialPassword cred

getBasicAuthFromCredential :: Repo -> TMVar CredentialCache -> GetBasicAuth
getBasicAuthFromCredential r ccv u respheaders = do
	(CredentialCache cc) <- atomically $ readTMVar ccv
	case mkCredentialBaseURL r u of
		Just bu -> case M.lookup bu cc of
			Just c -> go (const noop) c
			Nothing -> do
				let storeincache = \c -> atomically $ do
					CredentialCache cc' <- takeTMVar ccv
					putTMVar ccv (CredentialCache (M.insert bu c cc'))
				go storeincache =<< getUrlCredential u respheaders r
		Nothing -> go (const noop) =<< getUrlCredential u respheaders r
  where
	go storeincache c =
		case credentialBasicAuth c of
			Just ba -> return $ Just (ba, signalsuccess)
			Nothing -> do
				signalsuccess False
				return Nothing
	  where
		signalsuccess True = do
			() <- storeincache c
			approveUrlCredential c r
		signalsuccess False = rejectUrlCredential c r

-- | This may prompt the user for the credential, or get a cached
-- credential from git.
getUrlCredential :: URLString -> ResponseHeaders -> Repo -> IO Credential
getUrlCredential url respheaders = runCredential "fill" $ 
	urlCredential url respheaders

-- | Call if the credential the user entered works, and can be cached for
-- later use if git is configured to do so.
approveUrlCredential :: Credential -> Repo -> IO ()
approveUrlCredential c = void . runCredential "approve" c

-- | Call if the credential the user entered does not work.
rejectUrlCredential :: Credential -> Repo -> IO ()
rejectUrlCredential c = void . runCredential "reject" c

urlCredential :: URLString -> ResponseHeaders -> Credential
urlCredential url respheaders = Credential $ M.fromList $
	("url", url) : map wwwauth (filter iswwwauth respheaders)
  where
	iswwwauth (h, _) = h == hWWWAuthenticate
	wwwauth (_, v) = ("wwwauth[]", decodeBS v)

runCredential :: String -> Credential -> Repo -> IO Credential
runCredential action input r =
	parseCredential . decodeBS <$> pipeWriteRead 
		[ Param "credential"
		, Param action
		]
		(Just (flip hPutStr formatinput))
		r
  where
	formatinput = concat
		[ formatCredential input
		, "\n" -- blank line signifies end of input
		]

formatCredential :: Credential -> String
formatCredential = unlines . map (\(k, v) -> k ++"=" ++ v) . M.toList . fromCredential

parseCredential :: String -> Credential
parseCredential = Credential . M.fromList . map go . lines
  where
	go l = case break (== '=') l of
		(k, _:v) -> (k, v)
		(k, []) -> (k, "")

-- This is not the cache used by git, but is an in-process cache, 
-- allowing a process to avoid prompting repeatedly when accessing related
-- urls even when git is not configured to cache credentials.
data CredentialCache = CredentialCache (M.Map CredentialBaseURL Credential)

-- An url with the uriPath empty when credential.useHttpPath is false.
--
-- When credential.useHttpPath is true, no caching is done, since each 
-- distinct url would need a different credential to be cached, which
-- could cause the CredentialCache to use a lot of memory. Presumably,
-- when credential.useHttpPath is false, one Credential is cached
-- for each git repo accessed, and there are a reasonably small number of
-- those, so the cache will not grow too large.
data CredentialBaseURL
	= CredentialBaseURI URI
	| CredentialBaseURL String
	deriving (Show, Eq, Ord)

mkCredentialBaseURL :: Repo -> URLString -> Maybe CredentialBaseURL
mkCredentialBaseURL r s = do
	u <- parseURIPortable s
	let usehttppath = fromMaybe False $ Config.isTrueFalse' $
		Config.get (ConfigKey "credential.useHttpPath") (ConfigValue "") r
	if usehttppath
		then Nothing
		else Just $ CredentialBaseURI $ u { uriPath = "" }

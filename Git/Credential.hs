{- git credential interface
 -
 - Copyright 2019-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.Credential where

import Common
import Git
import Git.Command
import Utility.Url

import qualified Data.Map as M

data Credential = Credential { fromCredential :: M.Map String String }

credentialUsername :: Credential -> Maybe String
credentialUsername = M.lookup "username" . fromCredential

credentialPassword :: Credential -> Maybe String
credentialPassword = M.lookup "password" . fromCredential

credentialBasicAuth :: Credential -> Maybe BasicAuth
credentialBasicAuth cred = BasicAuth
	<$> credentialUsername cred
	<*> credentialPassword cred

getBasicAuthFromCredential :: Repo -> GetBasicAuth
getBasicAuthFromCredential r u = do
	c <- getUrlCredential u r
	case credentialBasicAuth c of
		Just ba -> return $ Just (ba, signalsuccess c)
		Nothing -> do
			signalsuccess c False
			return Nothing
  where
	signalsuccess c True = approveUrlCredential c r
	signalsuccess c False = rejectUrlCredential c r

-- | This may prompt the user for login information, or get cached login
-- information.
getUrlCredential :: URLString -> Repo -> IO Credential
getUrlCredential = runCredential "fill" . urlCredential

-- | Call if the credential the user entered works, and can be cached for
-- later use if git is configured to do so.
approveUrlCredential :: Credential -> Repo -> IO ()
approveUrlCredential c = void . runCredential "approve" c

-- | Call if the credential the user entered does not work.
rejectUrlCredential :: Credential -> Repo -> IO ()
rejectUrlCredential c = void . runCredential "reject" c

urlCredential :: URLString -> Credential
urlCredential = Credential . M.singleton "url"

runCredential :: String -> Credential -> Repo -> IO Credential
runCredential action input r =
	parseCredential <$> pipeWriteRead 
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

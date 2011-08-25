{- Url downloading.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Url (
	exists,
	download,
	get
) where

import Control.Applicative
import Control.Monad.State (liftIO)
import qualified Network.Browser as Browser
import Network.HTTP
import Network.URI

import Types
import Messages
import Utility.SafeCommand

type URLString = String

{- Checks that an url exists and could be successfully downloaded. -}
exists :: URLString -> IO Bool
exists url =
	case parseURI url of
		Nothing -> return False
		Just u -> do
			r <- request u HEAD
			case rspCode r of
				(2,_,_) -> return True
				_ -> return False

{- Used to download large files, such as the contents of keys.
 - Uses curl program for its progress bar. -}
download :: URLString -> FilePath -> Annex Bool
download url file = do
	showOutput -- make way for curl progress bar
	-- Uses the -# progress display, because the normal one is very
	-- confusing when resuming, showing the remainder to download
	-- as the whole file, and not indicating how much percent was
	-- downloaded before the resume.
	liftIO $ boolSystem "curl" [Params "-L -C - -# -o", File file, File url]

{- Downloads a small file. -}
get :: URLString -> IO String
get url =
	case parseURI url of
		Nothing -> error "url parse error"
		Just u -> do
			r <- request u GET
			case rspCode r of
				(2,_,_) -> return $ rspBody r
				_ -> error $ rspReason r

{- Makes a http request of an url. For example, HEAD can be used to
 - check if the url exists, or GET used to get the url content (best for
 - small urls). -}
request :: URI -> RequestMethod -> IO (Response String)
request url requesttype = Browser.browse $ do
	Browser.setErrHandler ignore
	Browser.setOutHandler ignore
	Browser.setAllowRedirects True
	snd <$> Browser.request (mkRequest requesttype url :: Request_String)
	where
		ignore = const $ return ()

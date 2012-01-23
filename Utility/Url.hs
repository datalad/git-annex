{- Url downloading.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Url (
	URLString,
	exists,
	canDownload,
	download,
	get
) where

import Control.Applicative
import qualified Network.Browser as Browser
import Network.HTTP
import Network.URI

import Utility.SafeCommand
import Utility.Path

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

canDownload :: IO Bool
canDownload = (||) <$> inPath "wget" <*> inPath "curl"

{- Used to download large files, such as the contents of keys.
 -
 - Uses wget or curl program for its progress bar. (Wget has a better one,
 - so is preferred.) Which program to use is determined at run time; it
 - would not be appropriate to test at configure time and build support
 - for only one in.
 -}
download :: URLString -> [CommandParam] -> FilePath -> IO Bool
download url options file = do
	e <- inPath "wget"
	if e
		then
			go "wget" [Params "-c -O", File file, File url]
		else
			-- Uses the -# progress display, because the normal
			-- one is very confusing when resuming, showing
			-- the remainder to download as the whole file,
			-- and not indicating how much percent was
			-- downloaded before the resume.
			go "curl" [Params "-L -C - -# -o", File file, File url]
	where
		go cmd opts = boolSystem cmd (options++opts)

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

{- Url downloading.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Url (
	URLString,
	check,
	exists,
	canDownload,
	download,
	get
) where

import Control.Applicative
import Control.Monad
import qualified Network.Browser as Browser
import Network.HTTP
import Network.URI
import Data.Maybe

import Utility.SafeCommand
import Utility.Path

type URLString = String

{- Checks that an url exists and could be successfully downloaded,
 - also checking that its size, if available, matches a specified size. -}
check :: URLString -> Maybe Integer -> IO Bool
check url expected_size = handle <$> exists url
	where
		handle (False, _) = False
		handle (True, Nothing) = True
		handle (True, s) = expected_size == s

{- Checks that an url exists and could be successfully downloaded,
 - also returning its size if available. -}
exists :: URLString -> IO (Bool, Maybe Integer)
exists url =
	case parseURI url of
		Nothing -> return (False, Nothing)
		Just u -> do
			r <- request u HEAD
			case rspCode r of
				(2,_,_) -> return (True, size r)
				_ -> return (False, Nothing)
	where
		size = liftM read . lookupHeader HdrContentLength . rspHeaders

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
 - small urls).
 -
 - This does its own redirect following because Browser's is buggy for HEAD
 - requests.
 -}
request :: URI -> RequestMethod -> IO (Response String)
request url requesttype = go 5 url
	where
		go :: Int -> URI -> IO (Response String)
		go 0 _ = error "Too many redirects "
		go n u = do
			rsp <- Browser.browse $ do
				Browser.setErrHandler ignore
				Browser.setOutHandler ignore
				Browser.setAllowRedirects False
				snd <$> Browser.request (mkRequest requesttype u :: Request_String)
			case rspCode rsp of
				(3,0,x) | x /= 5 -> redir (n - 1) u rsp
				_ -> return rsp
		ignore = const $ return ()
		redir n u rsp = do
			case retrieveHeaders HdrLocation rsp of
				[] -> return rsp
				(Header _ newu:_) ->
					case parseURIReference newu of
						Nothing -> return rsp
						Just newURI -> go n newURI_abs
							where
								newURI_abs = fromMaybe newURI (newURI `relativeTo` u)

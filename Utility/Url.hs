{- Url downloading.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.Url (
	URLString,
	check,
	exists,
	download,
	get
) where

import Common
import qualified Network.Browser as Browser
import Network.HTTP
import Network.URI
import Data.Either

type URLString = String

type Headers = [String]

{- Checks that an url exists and could be successfully downloaded,
 - also checking that its size, if available, matches a specified size. -}
check :: URLString -> Headers -> Maybe Integer -> IO Bool
check url headers expected_size = handle <$> exists url headers
  where
	handle (False, _) = False
	handle (True, Nothing) = True
	handle (True, s) = expected_size == s

{- Checks that an url exists and could be successfully downloaded,
 - also returning its size if available. -}
exists :: URLString -> Headers -> IO (Bool, Maybe Integer)
exists url headers = case parseURI url of
	Nothing -> return (False, Nothing)
	Just u
		| uriScheme u == "file:" -> do
			s <- catchMaybeIO $ getFileStatus (uriPath u)
			return $ case s of
				Nothing -> (False, Nothing)
				Just stat -> (True, Just $ fromIntegral $ fileSize stat)
		| otherwise -> do
			r <- request u headers HEAD
			case rspCode r of
				(2,_,_) -> return (True, size r)
				_ -> return (False, Nothing)
  where
	size = liftM Prelude.read . lookupHeader HdrContentLength . rspHeaders

{- Used to download large files, such as the contents of keys.
 -
 - Uses wget or curl program for its progress bar. (Wget has a better one,
 - so is preferred.) Which program to use is determined at run time; it
 - would not be appropriate to test at configure time and build support
 - for only one in.
 -
 - Curl is always used for file:// urls, as wget does not support them.
 -}
download :: URLString -> Headers -> [CommandParam] -> FilePath -> IO Bool
download url headers options file
	| "file://" `isPrefixOf` url = curl
	| otherwise = ifM (inPath "wget") (wget , curl)
  where
	headerparams = map (\h -> Param $ "--header=" ++ h) headers
	wget = go "wget" $ headerparams ++ [Params "-c -O"]
	{- Uses the -# progress display, because the normal
	 - one is very confusing when resuming, showing
	 - the remainder to download as the whole file,
	 - and not indicating how much percent was
	 - downloaded before the resume. -}
	curl = go "curl" $ headerparams ++ [Params "-L -C - -# -o"]
	go cmd opts = boolSystem cmd $
		options++opts++[File file, File url]

{- Downloads a small file. -}
get :: URLString -> Headers -> IO String
get url headers =
	case parseURI url of
		Nothing -> error "url parse error"
		Just u -> do
			r <- request u headers GET
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
request :: URI -> Headers -> RequestMethod -> IO (Response String)
request url headers requesttype = go 5 url
  where
	go :: Int -> URI -> IO (Response String)
	go 0 _ = error "Too many redirects "
	go n u = do
		rsp <- Browser.browse $ do
			Browser.setErrHandler ignore
			Browser.setOutHandler ignore
			Browser.setAllowRedirects False
			let req = mkRequest requesttype u :: Request_String
			snd <$> Browser.request (addheaders req)
		case rspCode rsp of
			(3,0,x) | x /= 5 -> redir (n - 1) u rsp
			_ -> return rsp
	ignore = const noop
	redir n u rsp = case retrieveHeaders HdrLocation rsp of
		[] -> return rsp
		(Header _ newu:_) ->
			case parseURIReference newu of
				Nothing -> return rsp
				Just newURI -> go n newURI_abs
				  where
#if defined VERSION_network
#if ! MIN_VERSION_network(2,4,0)
#define WITH_OLD_URI
#endif
#endif
#ifdef WITH_OLD_URI
					newURI_abs = fromMaybe newURI (newURI `relativeTo` u)
#else
					newURI_abs = newURI `relativeTo` u
#endif
	addheaders req = setHeaders req (rqHeaders req ++ userheaders)
	userheaders = rights $ map parseHeader headers

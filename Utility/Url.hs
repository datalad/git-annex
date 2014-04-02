{- Url downloading.
 -
 - Copyright 2011,2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.Url (
	URLString,
	UserAgent,
	UrlOptions(..),
	check,
	checkBoth,
	exists,
	download,
	downloadQuiet,
	parseURIRelaxed
) where

import Common
import Network.URI
import qualified Network.Browser as Browser
import Network.HTTP
import Data.Either
import Data.Default

import qualified Build.SysConfig

type URLString = String

type Headers = [String]

type UserAgent = String

data UrlOptions = UrlOptions
	{ userAgent :: Maybe UserAgent
	, reqHeaders :: Headers
	, reqParams :: [CommandParam]
	}

instance Default UrlOptions
  where
	def = UrlOptions Nothing [] []

{- Checks that an url exists and could be successfully downloaded,
 - also checking that its size, if available, matches a specified size. -}
checkBoth :: URLString -> Maybe Integer -> UrlOptions -> IO Bool
checkBoth url expected_size uo = do
	v <- check url expected_size uo
	return (fst v && snd v)
check :: URLString -> Maybe Integer -> UrlOptions -> IO (Bool, Bool)
check url expected_size = handle <$$> exists url
  where
	handle (False, _) = (False, False)
	handle (True, Nothing) = (True, True)
	handle (True, s) = case expected_size of
		Just _ -> (True, expected_size == s)
		Nothing -> (True, True)

{- Checks that an url exists and could be successfully downloaded,
 - also returning its size if available. 
 -
 - For a file: url, check it directly.
 -
 - Uses curl otherwise, when available, since curl handles https better
 - than does Haskell's Network.Browser.
 -}
exists :: URLString -> UrlOptions -> IO (Bool, Maybe Integer)
exists url uo = case parseURIRelaxed url of
	Just u
		| uriScheme u == "file:" -> do
			s <- catchMaybeIO $ getFileStatus (unEscapeString $ uriPath u)
			case s of
				Just stat -> return (True, Just $ fromIntegral $ fileSize stat)
				Nothing -> dne
		| otherwise -> if Build.SysConfig.curl
			then do
				output <- catchDefaultIO "" $
					readProcess "curl" $ toCommand curlparams
				case lastMaybe (lines output) of
					Just ('2':_:_) -> return (True, extractsize output)
					_ -> dne
			else do
				r <- request u HEAD uo
				case rspCode r of
					(2,_,_) -> return (True, size r)
					_ -> return (False, Nothing)
	Nothing -> dne
  where
	dne = return (False, Nothing)

	curlparams = addUserAgent uo $
		[ Param "-s"
		, Param "--head"
		, Param "-L", Param url
		, Param "-w", Param "%{http_code}"
		] ++ concatMap (\h -> [Param "-H", Param h]) (reqHeaders uo) ++ (reqParams uo)

	extractsize s = case lastMaybe $ filter ("Content-Length:" `isPrefixOf`) (lines s) of
		Just l -> case lastMaybe $ words l of
			Just sz -> readish sz
			_ -> Nothing
		_ -> Nothing

	size = liftM Prelude.read . lookupHeader HdrContentLength . rspHeaders

-- works for both wget and curl commands
addUserAgent :: UrlOptions -> [CommandParam] -> [CommandParam]
addUserAgent uo ps = case userAgent uo of
	Nothing -> ps
	Just ua -> ps ++ [Param "--user-agent", Param ua] 

{- Used to download large files, such as the contents of keys.
 -
 - Uses wget or curl program for its progress bar. (Wget has a better one,
 - so is preferred.) Which program to use is determined at run time; it
 - would not be appropriate to test at configure time and build support
 - for only one in.
 -}
download :: URLString -> FilePath -> UrlOptions -> IO Bool
download = download' False

{- No output, even on error. -}
downloadQuiet :: URLString -> FilePath -> UrlOptions -> IO Bool
downloadQuiet = download' True

download' :: Bool -> URLString -> FilePath -> UrlOptions -> IO Bool
download' quiet url file uo = 
	case parseURIRelaxed url of
		Just u
			| uriScheme u == "file:" -> do
				-- curl does not create destination file
				-- for an empty file:// url, so pre-create
				writeFile file ""
				curl
			| otherwise -> ifM (inPath "wget") (wget , curl)
		_ -> return False
  where
	headerparams = map (\h -> Param $ "--header=" ++ h) (reqHeaders uo)
	wget = go "wget" $ headerparams ++ quietopt "-q" ++ wgetparams
	{- Regular wget needs --clobber to continue downloading an existing
	 - file. On Android, busybox wget is used, which does not
	 - support, or need that option. -}
#ifndef __ANDROID__
	wgetparams = [Params "--clobber -c -O"]
#else
	wgetparams = [Params "-c -O"]
#endif
	{- Uses the -# progress display, because the normal
	 - one is very confusing when resuming, showing
	 - the remainder to download as the whole file,
	 - and not indicating how much percent was
	 - downloaded before the resume. -}
	curl = go "curl" $ headerparams ++ quietopt "-s" ++
		[Params "-f -L -C - -# -o"]
	go cmd opts = boolSystem cmd $
		addUserAgent uo $ reqParams uo++opts++[File file, File url]
	quietopt s
		| quiet = [Param s]
		| otherwise = []

{- Uses Network.Browser to make a http request of an url.
 - For example, HEAD can be used to check if the url exists,
 - or GET used to get the url content (best for small urls).
 -
 - This does its own redirect following because Browser's is buggy for HEAD
 - requests.
 -
 - Unfortunately, does not handle https, so should only be used
 - when curl is not available.
 -}
request :: URI -> RequestMethod -> UrlOptions -> IO (Response String)
request url requesttype uo = go 5 url
  where
	go :: Int -> URI -> IO (Response String)
	go 0 _ = error "Too many redirects "
	go n u = do
		rsp <- Browser.browse $ do
			maybe noop Browser.setUserAgent (userAgent uo)
			Browser.setErrHandler ignore
			Browser.setOutHandler ignore
			Browser.setAllowRedirects False
			let req = mkRequest requesttype u :: Request_String
			snd <$> Browser.request (addheaders req)
		case rspCode rsp of
			(3,0,x) | x /= 5 -> redir (n - 1) u rsp
			_ -> return rsp
	addheaders req = setHeaders req (rqHeaders req ++ userheaders)
	userheaders = rights $ map parseHeader (reqHeaders uo)
	ignore = const noop
	redir n u rsp = case retrieveHeaders HdrLocation rsp of
		[] -> return rsp
		(Header _ newu:_) ->
			case parseURIReference newu of
				Nothing -> return rsp
				Just newURI -> go n $
#if defined VERSION_network
#if ! MIN_VERSION_network(2,4,0)
#define WITH_OLD_URI
#endif
#endif
#ifdef WITH_OLD_URI
					fromMaybe newURI (newURI `relativeTo` u)
#else
					newURI `relativeTo` u
#endif

{- Allows for spaces and other stuff in urls, properly escaping them. -}
parseURIRelaxed :: URLString -> Maybe URI
parseURIRelaxed = parseURI . escapeURIString isAllowedInURI

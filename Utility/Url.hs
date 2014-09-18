{- Url downloading.
 -
 - Copyright 2011-2014 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Utility.Url (
	URLString,
	UserAgent,
	UrlOptions,
	mkUrlOptions,
	check,
	checkBoth,
	exists,
	download,
	downloadQuiet,
	parseURIRelaxed
) where

import Common
import Network.URI
import Network.HTTP.Conduit
import Network.HTTP.Types
import Data.Default
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B8

import qualified Build.SysConfig

type URLString = String

type Headers = [String]

type UserAgent = String

data UrlOptions = UrlOptions
	{ userAgent :: Maybe UserAgent
	, reqHeaders :: Headers
	, reqParams :: [CommandParam]
#if MIN_VERSION_http_conduit(2,0,0)
	, applyRequest :: Request -> Request
#else
	, applyRequest :: forall m. Request m -> Request m
#endif
	}

instance Default UrlOptions
  where
	def = UrlOptions Nothing [] [] id

mkUrlOptions :: Maybe UserAgent -> Headers -> [CommandParam] -> UrlOptions
mkUrlOptions useragent reqheaders reqparams =
	UrlOptions useragent reqheaders reqparams applyrequest
  where
	applyrequest = \r -> r { requestHeaders = requestHeaders r ++ addedheaders }
	addedheaders = uaheader ++ otherheaders
	uaheader = case useragent of
		Nothing -> []
		Just ua -> [(hUserAgent, B8.fromString ua)]
	otherheaders = map toheader reqheaders
	toheader s =
		let (h, v) = separate (== ':') s
		    h' = CI.mk (B8.fromString h)
		in case v of
			(' ':v') -> (h', B8.fromString v')
			_ -> (h', B8.fromString v)

addUserAgent :: UrlOptions -> [CommandParam] -> [CommandParam]
addUserAgent uo ps = case userAgent uo of
	Nothing -> ps
	-- --user-agent works for both wget and curl commands
	Just ua -> ps ++ [Param "--user-agent", Param ua] 

{- Checks that an url exists and could be successfully downloaded,
 - also checking that its size, if available, matches a specified size. -}
checkBoth :: URLString -> Maybe Integer -> UrlOptions -> IO Bool
checkBoth url expected_size uo = do
	v <- check url expected_size uo
	return (fst v && snd v)
check :: URLString -> Maybe Integer -> UrlOptions -> IO (Bool, Bool)
check url expected_size = go <$$> exists url
  where
	go (False, _) = (False, False)
	go (True, Nothing) = (True, True)
	go (True, s) = case expected_size of
		Just _ -> (True, expected_size == s)
		Nothing -> (True, True)

{- Checks that an url exists and could be successfully downloaded,
 - also returning its size if available. -}
exists :: URLString -> UrlOptions -> IO (Bool, Maybe Integer)
exists url uo = case parseURIRelaxed url of
	Just u -> case parseUrl (show u) of
		Just req -> existsconduit req `catchNonAsync` const dne
		-- http-conduit does not support file:, ftp:, etc urls,
		-- so fall back to reading files and using curl.
		Nothing
			| uriScheme u == "file:" -> do
				s <- catchMaybeIO $ getFileStatus (unEscapeString $ uriPath u)
				case s of
					Just stat -> return (True, Just $ fromIntegral $ fileSize stat)
					Nothing -> dne
			| Build.SysConfig.curl -> do
				output <- catchDefaultIO "" $
					readProcess "curl" $ toCommand curlparams
				case lastMaybe (lines output) of
					Just ('2':_:_) -> return (True, extractlencurl output)
					_ -> dne
			| otherwise -> dne
	Nothing -> dne
  where
	dne = return (False, Nothing)

	curlparams = addUserAgent uo $
		[ Param "-s"
		, Param "--head"
		, Param "-L", Param url
		, Param "-w", Param "%{http_code}"
		] ++ concatMap (\h -> [Param "-H", Param h]) (reqHeaders uo) ++ (reqParams uo)

	extractlencurl s = case lastMaybe $ filter ("Content-Length:" `isPrefixOf`) (lines s) of
		Just l -> case lastMaybe $ words l of
			Just sz -> readish sz
			_ -> Nothing
		_ -> Nothing
	
	extractlen resp = readish . B8.toString =<< headMaybe lenheaders
	  where
		lenheaders = map snd $ 
			filter (\(h, _) -> h == hContentLength)
				(responseHeaders resp)
	
	existsconduit req = withManager $ \mgr -> do
			let req' = headRequest (applyRequest uo req)
			resp <- http req' mgr
			-- forces processing the response before the
			-- manager is closed
			ret <- if responseStatus resp == ok200
				then return (True, extractlen resp)
				else liftIO dne
			liftIO $ closeManager mgr
			return ret

#if MIN_VERSION_http_conduit(2,0,0)
headRequest :: Request -> Request
#else
headRequest :: Request m -> Request m
#endif
headRequest r = r
	{ method = methodHead
	-- remove defaut Accept-Encoding header, to get actual,
	-- not gzip compressed size.
	, requestHeaders = (hAcceptEncoding, B.empty) :
		filter (\(h, _) -> h /= hAcceptEncoding)
		(requestHeaders r)
	}

addUrlOptions :: UrlOptions -> Request -> Request
addUrlOptions uo r = r { requestHeaders = requestHeaders r ++ uaheader ++ otherheaders}
  where
	uaheader = case userAgent uo of
		Nothing -> []
		Just ua -> [(hUserAgent, B8.fromString ua)]
	otherheaders = map toheader (reqHeaders uo)
	toheader s =
		let (h, v) = separate (== ':') s
		    h' = CI.mk (B8.fromString h)
		in case v of
			(' ':v') -> (h', B8.fromString v')
			_ -> (h', B8.fromString v)

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

{- Allows for spaces and other stuff in urls, properly escaping them. -}
parseURIRelaxed :: URLString -> Maybe URI
parseURIRelaxed = parseURI . escapeURIString isAllowedInURI

hAcceptEncoding :: CI.CI B.ByteString
hAcceptEncoding = "Accept-Encoding"

#if ! MIN_VERSION_http_types(0,7,0)
hContentLength :: CI.CI B.ByteString
hContentLength = "Content-Length"

hUserAgent :: CI.CI B.ByteString
hUserAgent = "User-Agent"
#endif

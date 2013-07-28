{- Url downloading.
 -
 - Copyright 2011,2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.Url (
	URLString,
	check,
	exists,
	download,
	downloadQuiet
) where

import Common
import Network.URI
import qualified Network.Browser as Browser
import Network.HTTP
import Data.Either

import qualified Build.SysConfig

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
 - also returning its size if available. 
 -
 - For a file: url, check it directly.
 -
 - Uses curl otherwise, when available, since curl handles https better
 - than does Haskell's Network.Browser.
 -}
exists :: URLString -> Headers -> IO (Bool, Maybe Integer)
exists url headers = case parseURIRelaxed url of
	Just u
		| uriScheme u == "file:" -> do
			s <- catchMaybeIO $ getFileStatus (unEscapeString $ uriPath u)
			case s of
				Just stat -> return (True, Just $ fromIntegral $ fileSize stat)
				Nothing -> dne
		| otherwise -> if Build.SysConfig.curl
			then do
				output <- readProcess "curl" curlparams
				case lastMaybe (lines output) of
					Just ('2':_:_) -> return (True, extractsize output)
					_ -> dne
			else do
				r <- request u headers HEAD
				case rspCode r of
					(2,_,_) -> return (True, size r)
					_ -> return (False, Nothing)
	Nothing -> dne
  where
	dne = return (False, Nothing)

	curlparams = 
		[ "-s"
		, "--head"
		, "-L"
		, url
		, "-w", "%{http_code}"
		] ++ concatMap (\h -> ["-H", h]) headers

	extractsize s = case lastMaybe $ filter ("Content-Length:" `isPrefixOf`) (lines s) of
		Just l -> case lastMaybe $ words l of
			Just sz -> readish sz
			_ -> Nothing
		_ -> Nothing

	size = liftM Prelude.read . lookupHeader HdrContentLength . rspHeaders

{- Used to download large files, such as the contents of keys.
 -
 - Uses wget or curl program for its progress bar. (Wget has a better one,
 - so is preferred.) Which program to use is determined at run time; it
 - would not be appropriate to test at configure time and build support
 - for only one in.
 -}
download :: URLString -> Headers -> [CommandParam] -> FilePath -> IO Bool
download = download' False

{- No output, even on error. -}
downloadQuiet :: URLString -> Headers -> [CommandParam] -> FilePath -> IO Bool
downloadQuiet = download' True

download' :: Bool -> URLString -> Headers -> [CommandParam] -> FilePath -> IO Bool
download' quiet url headers options file = 
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
	headerparams = map (\h -> Param $ "--header=" ++ h) headers
	wget = go "wget" $ headerparams ++ quietopt "-q" ++ [Params "-c -O"]
	{- Uses the -# progress display, because the normal
	 - one is very confusing when resuming, showing
	 - the remainder to download as the whole file,
	 - and not indicating how much percent was
	 - downloaded before the resume. -}
	curl = go "curl" $ headerparams ++ quietopt "-s" ++
		[Params "-f -L -C - -# -o"]
	go cmd opts = boolSystem cmd $
		options++opts++[File file, File url]
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
	addheaders req = setHeaders req (rqHeaders req ++ userheaders)
	userheaders = rights $ map parseHeader headers
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

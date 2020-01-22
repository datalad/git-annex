{- Url downloading.
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Utility.Url (
	newManager,
	URLString,
	UserAgent,
	Scheme,
	mkScheme,
	allowedScheme,
	UrlDownloader(..),
	NonHttpUrlDownloader(..),
	UrlOptions(..),
	defUrlOptions,
	mkUrlOptions,
	check,
	checkBoth,
	exists,
	UrlInfo(..),
	getUrlInfo,
	assumeUrlExists,
	download,
	downloadQuiet,
	downloadConduit,
	sinkResponseFile,
	downloadPartial,
	parseURIRelaxed,
	matchStatusCodeException,
	matchHttpExceptionContent,
	BasicAuth(..),
	GetBasicAuth,
	noBasicAuth,
	applyBasicAuth',
) where

import Common
import Utility.Metered
import Utility.HttpManagerRestricted
import Utility.IPAddress

import Network.URI
import Network.HTTP.Types
import qualified Network.Connection as NC
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.Set as S
import Control.Exception (throwIO)
import Control.Monad.Trans.Resource
import Network.HTTP.Conduit
import Network.HTTP.Client
import Network.HTTP.Simple (getResponseHeader)
import Network.Socket
import Network.BSD (getProtocolNumber)
import Data.Either
import Data.Conduit
import Text.Read
import System.Log.Logger

type URLString = String

type Headers = [String]

type UserAgent = String

newtype Scheme = Scheme (CI.CI String)
	deriving (Eq, Ord)

mkScheme :: String -> Scheme
mkScheme = Scheme . CI.mk

fromScheme :: Scheme -> String
fromScheme (Scheme s) = CI.original s

data UrlOptions = UrlOptions
	{ userAgent :: Maybe UserAgent
	, reqHeaders :: Headers
	, urlDownloader :: UrlDownloader
	, applyRequest :: Request -> Request
	, httpManager :: Manager
	, allowedSchemes :: S.Set Scheme
	, getBasicAuth :: GetBasicAuth
	}

data UrlDownloader
	= DownloadWithConduit NonHttpUrlDownloader
	| DownloadWithCurl [CommandParam]

data NonHttpUrlDownloader
	= DownloadWithCurlRestricted Restriction

defUrlOptions :: IO UrlOptions
defUrlOptions = UrlOptions
	<$> pure Nothing
	<*> pure []
	<*> pure (DownloadWithConduit (DownloadWithCurlRestricted mempty))
	<*> pure id
	<*> newManager tlsManagerSettings
	<*> pure (S.fromList $ map mkScheme ["http", "https", "ftp"])
	<*> pure noBasicAuth

mkUrlOptions :: Maybe UserAgent -> Headers -> UrlDownloader -> Manager -> S.Set Scheme -> GetBasicAuth -> UrlOptions
mkUrlOptions defuseragent reqheaders urldownloader manager getbasicauth =
	UrlOptions useragent reqheaders urldownloader applyrequest manager getbasicauth
  where
	applyrequest = \r -> r { requestHeaders = requestHeaders r ++ addedheaders }
	addedheaders = uaheader ++ otherheaders
	useragent = maybe defuseragent (Just . B8.toString . snd)
		(headMaybe uafromheaders)
	uaheader = case useragent of
		Nothing -> []
		Just ua -> [(hUserAgent, B8.fromString ua)]
	(uafromheaders, otherheaders) = partition (\(h, _) -> h == hUserAgent)
		(map toheader reqheaders)
	toheader s =
		let (h, v) = separate (== ':') s
		    h' = CI.mk (B8.fromString h)
		in case v of
			(' ':v') -> (h', B8.fromString v')
			_ -> (h', B8.fromString v)

curlParams :: UrlOptions -> [CommandParam] -> [CommandParam]
curlParams uo ps = ps ++ uaparams ++ headerparams ++ addedparams ++ schemeparams
  where
	uaparams = case userAgent uo of
		Nothing -> []
		Just ua -> [Param "--user-agent", Param ua]
	headerparams = concatMap (\h -> [Param "-H", Param h]) (reqHeaders uo)
	addedparams = case urlDownloader uo of
		DownloadWithConduit _ -> []
		DownloadWithCurl l -> l
	schemeparams =
		[ Param "--proto"
		, Param $ intercalate "," ("-all" : schemelist)
		]
	schemelist = map fromScheme $ S.toList $ allowedSchemes uo

checkPolicy :: UrlOptions -> URI -> IO (Either String a) -> IO (Either String a)
checkPolicy uo u a
	| allowedScheme uo u = a
	| otherwise = return $ Left $
		"Configuration does not allow accessing " ++ show u

unsupportedUrlScheme :: URI -> String
unsupportedUrlScheme u = "Unsupported url scheme " ++ show u

allowedScheme :: UrlOptions -> URI -> Bool
allowedScheme uo u = uscheme `S.member` allowedSchemes uo
  where
	uscheme = mkScheme $ takeWhile (/=':') (uriScheme u)

{- Checks that an url exists and could be successfully downloaded,
 - also checking that its size, if available, matches a specified size.
 -
 - The Left error is returned if policy does not allow accessing the url
 - or the url scheme is not supported.
 -}
checkBoth :: URLString -> Maybe Integer -> UrlOptions -> IO (Either String Bool)
checkBoth url expected_size uo = fmap go <$> check url expected_size uo
  where
	go v = fst v && snd v

check :: URLString -> Maybe Integer -> UrlOptions -> IO (Either String (Bool, Bool))
check url expected_size uo = fmap go <$> getUrlInfo url uo
  where
	go (UrlInfo False _ _) = (False, False)
	go (UrlInfo True Nothing _) = (True, True)
	go (UrlInfo True s _) = case expected_size of
		Just _ -> (True, expected_size == s)
		Nothing -> (True, True)

exists :: URLString -> UrlOptions -> IO (Either String Bool)
exists url uo = fmap urlExists <$> getUrlInfo url uo

data UrlInfo = UrlInfo
	{ urlExists :: Bool
	, urlSize :: Maybe Integer
	, urlSuggestedFile :: Maybe FilePath
	}
	deriving (Show)

assumeUrlExists :: UrlInfo
assumeUrlExists = UrlInfo True Nothing Nothing

{- Checks that an url exists and could be successfully downloaded,
 - also returning its size and suggested filename if available.
 -
 - The Left error is returned if policy does not allow accessing the url
 - or the url scheme is not supported.
 -}
getUrlInfo :: URLString -> UrlOptions -> IO (Either String UrlInfo)
getUrlInfo url uo = case parseURIRelaxed url of
	Just u -> checkPolicy uo u (go u)
	Nothing -> return (Right dne)
   where
	go :: URI -> IO (Either String UrlInfo)
	go u = case (urlDownloader uo, parseRequest (show u)) of
		(DownloadWithConduit (DownloadWithCurlRestricted r), Just req) -> catchJust
			-- When http redirects to a protocol which 
			-- conduit does not support, it will throw
			-- a StatusCodeException with found302
			-- and a Response with the redir Location.
			(matchStatusCodeException (== found302))
			(Right <$> existsconduit req uo)
			(followredir r)
				`catchNonAsync` (const $ return $ Right dne)
		(DownloadWithConduit (DownloadWithCurlRestricted r), Nothing)
			| isfileurl u -> Right <$> existsfile u
			| isftpurl u -> (Right <$> existscurlrestricted r u url ftpport)
				`catchNonAsync` (const $ return $ Right dne)
			| otherwise -> return $ Left $ unsupportedUrlScheme u
		(DownloadWithCurl _, _) 
			| isfileurl u -> Right <$> existsfile u
			| otherwise -> Right <$> existscurl u (basecurlparams url)
	
	dne = UrlInfo False Nothing Nothing
	found sz f = return $ UrlInfo True sz f

	isfileurl u = uriScheme u == "file:"
	isftpurl u = uriScheme u == "ftp:"

	ftpport = 21

	basecurlparams u = curlParams uo $
		[ Param "-s"
		, Param "--head"
		, Param "-L", Param u
		, Param "-w", Param "%{http_code}"
		]

	extractlencurl s = case lastMaybe $ filter ("Content-Length:" `isPrefixOf`) (lines s) of
		Just l -> case lastMaybe $ words l of
			Just sz -> readish sz
			_ -> Nothing
		_ -> Nothing
	
	extractlen = readish . B8.toString
		<=< lookup hContentLength . responseHeaders

	extractfilename = contentDispositionFilename . B8.toString
		<=< lookup hContentDisposition . responseHeaders

	existsconduit req uo' = do
		let req' = headRequest (applyRequest uo req)
		debugM "url" (show req')
		join $ runResourceT $ do
			resp <- http req' (httpManager uo)
			-- forces processing the response while
			-- within the runResourceT
			liftIO $ if responseStatus resp == ok200
				then do
					let !len = extractlen resp
					let !fn = extractfilename resp
					return $ found len fn
				else if responseStatus resp == unauthorized401
					then return $ getBasicAuth uo' (show (getUri req)) >>= \case
						Nothing -> return dne
						Just (ba, signalsuccess) -> do
							ui <- existsconduit
								(applyBasicAuth' ba req)							
								(uo' { getBasicAuth = noBasicAuth })
							signalsuccess (urlExists ui)
							return ui
					else return $ return dne

	existscurl u curlparams = do
		output <- catchDefaultIO "" $
			readProcess "curl" $ toCommand curlparams
		let len = extractlencurl output
		let good = found len Nothing
		let isftp = or
			[ "ftp" `isInfixOf` uriScheme u
			-- Check to see if http redirected to ftp.
			, "Location: ftp://" `isInfixOf` output
			]
		case lastMaybe (lines output) of
			Just ('2':_:_) -> good
			-- don't try to parse ftp status codes; if curl
			-- got a length, it's good
			_ | isftp && isJust len -> good
			_ -> return dne
	
	existscurlrestricted r u url' defport = existscurl u 
		=<< curlRestrictedParams r u defport (basecurlparams url')

	existsfile u = do
		let f = unEscapeString (uriPath u)
		s <- catchMaybeIO $ getFileStatus f
		case s of
			Just stat -> do
				sz <- getFileSize' f stat
				found (Just sz) Nothing
			Nothing -> return dne
	
	followredir r (HttpExceptionRequest _ (StatusCodeException resp _)) = 
		case headMaybe $ map decodeBS $ getResponseHeader hLocation resp of
			Just url' -> case parseURIRelaxed url' of
				-- only follow http to ftp redirects;
				-- http to file redirect would not be secure,
				-- and http-conduit follows http to http.
				Just u' | isftpurl u' ->
					checkPolicy uo u' $ Right <$> 
						existscurlrestricted r u' url' ftpport
				_ -> return (Right dne)
			Nothing -> return (Right dne)
	followredir _ _ = return (Right dne)

-- Parse eg: attachment; filename="fname.ext"
-- per RFC 2616
contentDispositionFilename :: String -> Maybe FilePath
contentDispositionFilename s
	| "attachment; filename=\"" `isPrefixOf` s && "\"" `isSuffixOf` s =
		Just $ dropFromEnd 1 $ drop 1 $ dropWhile (/= '"') s
	| otherwise = Nothing

headRequest :: Request -> Request
headRequest r = r
	{ method = methodHead
	-- remove defaut Accept-Encoding header, to get actual,
	-- not gzip compressed size.
	, requestHeaders = (hAcceptEncoding, B.empty) :
		filter (\(h, _) -> h /= hAcceptEncoding)
		(requestHeaders r)
	}

{- Download a perhaps large file, with auto-resume of incomplete downloads.
 -
 - When the download fails, returns an error message.
 -}
download :: MeterUpdate -> URLString -> FilePath -> UrlOptions -> IO (Either String ())
download = download' False

{- Avoids displaying any error message, including silencing curl errors. -}
downloadQuiet :: MeterUpdate -> URLString -> FilePath -> UrlOptions -> IO Bool
downloadQuiet meterupdate url file uo = isRight 
	<$> download' True meterupdate url file uo

download' :: Bool -> MeterUpdate -> URLString -> FilePath -> UrlOptions -> IO (Either String ())
download' nocurlerror meterupdate url file uo =
	catchJust matchHttpException go showhttpexception
		`catchNonAsync` (dlfailed . show)
  where
	go = case parseURIRelaxed url of
		Just u -> checkPolicy uo u $
			case (urlDownloader uo, parseRequest (show u)) of
				(DownloadWithConduit (DownloadWithCurlRestricted r), Just req) -> catchJust
					(matchStatusCodeException (== found302))
					(downloadConduit meterupdate req file uo >> return (Right ()))
					(followredir r)
				(DownloadWithConduit (DownloadWithCurlRestricted r), Nothing)
					| isfileurl u -> downloadfile u
					| isftpurl u -> downloadcurlrestricted r u url ftpport
					| otherwise -> dlfailed $ unsupportedUrlScheme u
				(DownloadWithCurl _, _)
					| isfileurl u -> downloadfile u
					| otherwise -> downloadcurl url basecurlparams
		Nothing -> do
			liftIO $ debugM "url" url
			dlfailed "invalid url"
	
	isfileurl u = uriScheme u == "file:"
	isftpurl u = uriScheme u == "ftp:"

	ftpport = 21

	showhttpexception he = dlfailed $ case he of
		HttpExceptionRequest _ (StatusCodeException r _) ->
			B8.toString $ statusMessage $ responseStatus r
		HttpExceptionRequest _ (InternalException ie) -> 
			case fromException ie of
				Nothing -> show ie
				Just (ConnectionRestricted why) -> why
		HttpExceptionRequest _ other -> show other
		_ -> show he
	
	dlfailed msg = return $ Left $ "download failed: " ++ msg

	basecurlparams = curlParams uo
		[ if nocurlerror
			then Param "-S"
			else Param "-sS"
		, Param "-f"
		, Param "-L"
		, Param "-C", Param "-"
		]

	downloadcurl rawurl curlparams = do
		-- curl does not create destination file
		-- if the url happens to be empty, so pre-create.
		unlessM (doesFileExist file) $
			writeFile file ""
		ifM (boolSystem "curl" (curlparams ++ [Param "-o", File file, File rawurl]))
			( return $ Right ()
			, return $ Left "download failed"
			)

	downloadcurlrestricted r u rawurl defport =
		downloadcurl rawurl =<< curlRestrictedParams r u defport basecurlparams

	downloadfile u = do
		let src = unEscapeString (uriPath u)
		withMeteredFile src meterupdate $
			L.writeFile file
		return $ Right ()

	-- Conduit does not support ftp, so will throw an exception on a
	-- redirect to a ftp url; fall back to curl.
	followredir r ex@(HttpExceptionRequest _ (StatusCodeException resp _)) = 
		case headMaybe $ map decodeBS $ getResponseHeader hLocation resp of
			Just url' -> case parseURIRelaxed url' of
				Just u' | isftpurl u' ->
					checkPolicy uo u' $
						downloadcurlrestricted r u' url' ftpport
				_ -> throwIO ex
			Nothing -> throwIO ex
	followredir _ ex = throwIO ex

{- Download a perhaps large file using conduit, with auto-resume
 - of incomplete downloads.
 -
 - Does not catch exceptions.
 -}
downloadConduit :: MeterUpdate -> Request -> FilePath -> UrlOptions -> IO ()
downloadConduit meterupdate req file uo =
	catchMaybeIO (getFileSize file) >>= \case
		Just sz | sz > 0 -> resumedownload sz
		_ -> join $ runResourceT $ do
			liftIO $ debugM "url" (show req')
			resp <- http req' (httpManager uo)
			if responseStatus resp == ok200
				then do
					store zeroBytesProcessed WriteMode resp
					return (return ())
				else if responseStatus resp == unauthorized401
					then return $ getBasicAuth uo (show (getUri req')) >>= \case
						Nothing -> respfailure resp
						Just ba -> retryauthed ba
					else return $ respfailure resp
  where
	req' = applyRequest uo $ req
		-- Override http-client's default decompression of gzip
		-- compressed files. We want the unmodified file content.
		{ requestHeaders = (hAcceptEncoding, "identity") :
			filter ((/= hAcceptEncoding) . fst)
				(requestHeaders req)
		, decompress = const False
		}

	-- Resume download from where a previous download was interrupted, 
	-- when supported by the http server. The server may also opt to
	-- send the whole file rather than resuming.
	resumedownload sz = catchJust
		(matchStatusCodeHeadersException (alreadydownloaded sz))
		dl
		(const noop)
	  where
		dl = join $ runResourceT $ do
			let req'' = req' { requestHeaders = resumeFromHeader sz : requestHeaders req }
			liftIO $ debugM "url" (show req'')
			resp <- http req'' (httpManager uo)
			if responseStatus resp == partialContent206
				then do
					store (toBytesProcessed sz) AppendMode resp
					return (return ())
				else if responseStatus resp == ok200
					then do
						store zeroBytesProcessed WriteMode resp
						return (return ())
					else if responseStatus resp == unauthorized401
						then return $ getBasicAuth uo (show (getUri req'')) >>= \case
							Nothing -> respfailure resp
							Just ba -> retryauthed ba
						else return $ respfailure resp
	
	alreadydownloaded sz s h = s == requestedRangeNotSatisfiable416 
		&& case lookup hContentRange h of
			-- This could be improved by fixing
			-- https://github.com/aristidb/http-types/issues/87
			Just crh -> crh == B8.fromString ("bytes */" ++ show sz)
			-- Some http servers send no Content-Range header when
			-- the range extends beyond the end of the file.
			-- There is no way to distinguish between the file
			-- being the same size on the http server, vs
			-- it being shorter than the file we already have.
			-- So assume we have the whole content of the file
			-- already, the same as wget and curl do.
			Nothing -> True
	
	store initialp mode resp =
		sinkResponseFile meterupdate initialp file mode resp
	
	respfailure = giveup . B8.toString . statusMessage . responseStatus
	
	retryauthed (ba, signalsuccess) = do
		r <- tryNonAsync $ downloadConduit
			meterupdate
			(applyBasicAuth' ba req)
			file
			(uo { getBasicAuth = noBasicAuth })
		case r of
			Right () -> signalsuccess True
			Left e -> do
				signalsuccess False
				throwM e
	
{- Sinks a Response's body to a file. The file can either be opened in
 - WriteMode or AppendMode. Updates the meter as data is received.
 -
 - Note that the responseStatus is not checked by this function.
 -}
sinkResponseFile
	:: MonadResource m
	=> MeterUpdate
	-> BytesProcessed
	-> FilePath
	-> IOMode
	-> Response (ConduitM () B8.ByteString m ())
	-> m ()
sinkResponseFile meterupdate initialp file mode resp = do
	(fr, fh) <- allocate (openBinaryFile file mode) hClose
	runConduit $ responseBody resp .| go initialp fh
	release fr
  where
	go sofar fh = await >>= \case
		Nothing -> return ()
		Just bs -> do
			let sofar' = addBytesProcessed sofar (B.length bs)
			liftIO $ do
				void $ meterupdate sofar'
				B.hPut fh bs
			go sofar' fh

{- Downloads at least the specified number of bytes from an url. -}
downloadPartial :: URLString -> UrlOptions -> Int -> IO (Maybe L.ByteString)
downloadPartial url uo n = case parseURIRelaxed url of
	Nothing -> return Nothing
	Just u -> go u `catchNonAsync` const (return Nothing)
  where
	go u = case parseRequest (show u) of
		Nothing -> return Nothing
		Just req -> do
			let req' = applyRequest uo req
			liftIO $ debugM "url" (show req')
			withResponse req' (httpManager uo) $ \resp ->
				if responseStatus resp == ok200
					then Just <$> brReadSome (responseBody resp) n
					else return Nothing

{- Allows for spaces and other stuff in urls, properly escaping them. -}
parseURIRelaxed :: URLString -> Maybe URI
parseURIRelaxed s = maybe (parseURIRelaxed' s) Just $
	parseURI $ escapeURIString isAllowedInURI s

{- Some characters like '[' are allowed in eg, the address of
 - an uri, but cannot appear unescaped further along in the uri.
 - This handles that, expensively, by successively escaping each character
 - from the back of the url until the url parses.
 -}
parseURIRelaxed' :: URLString -> Maybe URI
parseURIRelaxed' s = go [] (reverse s)
  where
	go back [] = parseURI back
	go back (c:cs) = case parseURI (escapeURIString isAllowedInURI (reverse (c:cs)) ++ back) of
		Just u -> Just u
		Nothing -> go (escapeURIChar escapemore c ++ back) cs

	escapemore '[' = False
	escapemore ']' = False
	escapemore c = isAllowedInURI c

hAcceptEncoding :: CI.CI B.ByteString
hAcceptEncoding = "Accept-Encoding"

hContentDisposition :: CI.CI B.ByteString
hContentDisposition = "Content-Disposition"

hContentRange :: CI.CI B.ByteString
hContentRange = "Content-Range"

resumeFromHeader :: FileSize -> Header
resumeFromHeader sz = (hRange, renderByteRanges [ByteRangeFrom sz])

{- Use with eg:
 -
 - > catchJust (matchStatusCodeException (== notFound404))
 -}
matchStatusCodeException :: (Status -> Bool) -> HttpException -> Maybe HttpException
matchStatusCodeException want = matchStatusCodeHeadersException (\s _h -> want s)

matchStatusCodeHeadersException :: (Status -> ResponseHeaders -> Bool) -> HttpException -> Maybe HttpException
matchStatusCodeHeadersException want e@(HttpExceptionRequest _ (StatusCodeException r _))
	| want (responseStatus r) (responseHeaders r) = Just e
	| otherwise = Nothing
matchStatusCodeHeadersException _ _ = Nothing

{- Use with eg: 
 -
 - > catchJust matchHttpException
 -}
matchHttpException :: HttpException -> Maybe HttpException
matchHttpException = Just

matchHttpExceptionContent :: (HttpExceptionContent -> Bool) -> HttpException -> Maybe HttpException
matchHttpExceptionContent want e@(HttpExceptionRequest _ hec)
	| want hec = Just e
	| otherwise = Nothing
matchHttpExceptionContent _ _ = Nothing

{- Constructs parameters that prevent curl from accessing any IP addresses
 - blocked by the Restriction. These are added to the input parameters,
 - which should tell curl what to do.
 -
 - This has to disable redirects because it looks up the IP addresses 
 - of the host and after limiting to those allowed by the Restriction,
 - makes curl resolve the host to those IP addresses. It doesn't make sense
 - to use this for http anyway, only for ftp or perhaps other protocols
 - supported by curl.
 -
 - Throws an exception if the Restriction blocks all addresses, or
 - if the dns lookup fails. A malformed url will also cause an exception.
 -}
curlRestrictedParams :: Restriction -> URI -> Int -> [CommandParam] -> IO [CommandParam]
curlRestrictedParams r u defport ps = case uriAuthority u of
	Nothing -> giveup "malformed url"
	Just uath -> case uriPort uath of
		"" -> go (uriRegName uath) defport
		-- strict parser because the port we provide to curl
		-- needs to match the port in the url
		(':':s) -> case readMaybe s :: Maybe Int of
			Just p -> go (uriRegName uath) p
			Nothing -> giveup "malformed url"
		_ -> giveup "malformed url"
  where
	go hostname p = do
		proto <- getProtocolNumber "tcp"
		let serv = show p
		let hints = defaultHints
			{ addrFlags = [AI_ADDRCONFIG]
			, addrProtocol = proto
			, addrSocketType = Stream
			}
		addrs <- getAddrInfo (Just hints) (Just hostname) (Just serv)
		case partitionEithers (map checkrestriction addrs) of
			((e:_es), []) -> throwIO e
			(_, as)
				| null as -> throwIO $ 
					NC.HostNotResolved hostname
				| otherwise -> return $
					(limitresolve p) as ++ ps
	checkrestriction addr = maybe (Right addr) Left $
		checkAddressRestriction r addr
	limitresolve p addrs =
		[ Param "--resolve"
		, Param $ "*:" ++ show p ++ ":" ++ intercalate ":"
			(mapMaybe (bracketaddr <$$> extractIPAddress . addrAddress) addrs)
		-- Don't let a ftp server provide an IP address.
		, Param "--ftp-skip-pasv-ip"
		-- Prevent all http redirects.
		, Param "--max-redirs", Param "0"
		]
	bracketaddr a = "[" ++ a ++ "]"

data BasicAuth = BasicAuth
	{ basicAuthUser :: String
	, basicAuthPassword :: String
	}

-- Note that this is only used when using conduit, not curl.
--
-- The returned IO action is run after trying to use the BasicAuth,
-- indicating if the password worked.
type GetBasicAuth = URLString -> IO (Maybe (BasicAuth, Bool -> IO ()))

noBasicAuth :: GetBasicAuth
noBasicAuth = const $ pure Nothing

applyBasicAuth' :: BasicAuth -> Request -> Request
applyBasicAuth' ba = applyBasicAuth
	(encodeBS (basicAuthUser ba))
	(encodeBS (basicAuthPassword ba))

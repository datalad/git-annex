{- Url downloading.
 -
 - Copyright 2011-2018 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Utility.Url (
	newManager,
	managerSettings,
	URLString,
	UserAgent,
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
	sinkResponseFile,
	downloadPartial,
	parseURIRelaxed,
	matchStatusCodeException,
	matchHttpExceptionContent,
) where

import Common
import Utility.Metered

import Network.URI
import Network.HTTP.Types
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString.Lazy as L
import Control.Monad.Trans.Resource
import Network.HTTP.Conduit
import Network.HTTP.Client (brRead, withResponse)
import Data.Conduit

#if ! MIN_VERSION_http_client(0,5,0)
responseTimeoutNone :: Maybe Int
responseTimeoutNone = Nothing
#endif

managerSettings :: ManagerSettings
#if MIN_VERSION_http_conduit(2,1,7)
managerSettings = tlsManagerSettings
#else
managerSettings = conduitManagerSettings
#endif
	{ managerResponseTimeout = responseTimeoutNone }

type URLString = String

type Headers = [String]

type UserAgent = String

data UrlOptions = UrlOptions
	{ userAgent :: Maybe UserAgent
	, reqHeaders :: Headers
	, urlDownloader :: UrlDownloader
	, applyRequest :: Request -> Request
	, httpManager :: Manager
	}

data UrlDownloader
	= DownloadWithConduit
	| DownloadWithCurl [CommandParam]

defUrlOptions :: IO UrlOptions
defUrlOptions = UrlOptions
	<$> pure Nothing
	<*> pure []
	<*> pure DownloadWithConduit
	<*> pure id
	<*> newManager managerSettings

mkUrlOptions :: Maybe UserAgent -> Headers -> [CommandParam] -> Manager -> UrlOptions
mkUrlOptions defuseragent reqheaders reqparams manager =
	UrlOptions useragent reqheaders urldownloader applyrequest manager
  where
	urldownloader = if null reqparams
		then DownloadWithConduit
		else DownloadWithCurl reqparams
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
curlParams uo ps = ps ++ uaparams ++ headerparams ++ addedparams
  where
	uaparams = case userAgent uo of
		Nothing -> []
		Just ua -> [Param "--user-agent", Param ua]
	headerparams = concatMap (\h -> [Param "-H", Param h]) (reqHeaders uo)
	addedparams = case urlDownloader uo of
		DownloadWithConduit -> []
		DownloadWithCurl l -> l

{- Checks that an url exists and could be successfully downloaded,
 - also checking that its size, if available, matches a specified size. -}
checkBoth :: URLString -> Maybe Integer -> UrlOptions -> IO Bool
checkBoth url expected_size uo = do
	v <- check url expected_size uo
	return (fst v && snd v)

check :: URLString -> Maybe Integer -> UrlOptions -> IO (Bool, Bool)
check url expected_size uo = go <$> getUrlInfo url uo
  where
	go (UrlInfo False _ _) = (False, False)
	go (UrlInfo True Nothing _) = (True, True)
	go (UrlInfo True s _) = case expected_size of
		Just _ -> (True, expected_size == s)
		Nothing -> (True, True)

exists :: URLString -> UrlOptions -> IO Bool
exists url uo = urlExists <$> getUrlInfo url uo

data UrlInfo = UrlInfo
	{ urlExists :: Bool
	, urlSize :: Maybe Integer
	, urlSuggestedFile :: Maybe FilePath
	}
	deriving (Show)

assumeUrlExists :: UrlInfo
assumeUrlExists = UrlInfo True Nothing Nothing

{- Checks that an url exists and could be successfully downloaded,
 - also returning its size and suggested filename if available. -}
getUrlInfo :: URLString -> UrlOptions -> IO UrlInfo
getUrlInfo url uo = case parseURIRelaxed url of
	Just u -> case (urlDownloader uo, parseUrlConduit (show u)) of
		(DownloadWithConduit, Just req) -> catchJust
			-- When http redirects to a protocol which 
			-- conduit does not support, it will throw
			-- a StatusCodeException with found302.
			(matchStatusCodeException (== found302))
			(existsconduit req)
			(const (existscurl u))
			`catchNonAsync` (const dne)
		-- http-conduit does not support file:, ftp:, etc urls,
		-- so fall back to reading files and using curl.
		_
			| uriScheme u == "file:" -> do
				let f = unEscapeString (uriPath u)
				s <- catchMaybeIO $ getFileStatus f
				case s of
					Just stat -> do
						sz <- getFileSize' f stat
						found (Just sz) Nothing
					Nothing -> dne
			| otherwise -> existscurl u
	Nothing -> dne
  where
	dne = return $ UrlInfo False Nothing Nothing
	found sz f = return $ UrlInfo True sz f

	curlparams = curlParams uo $
		[ Param "-s"
		, Param "--head"
		, Param "-L", Param url
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

	existsconduit req = do
		let req' = headRequest (applyRequest uo req)
		runResourceT $ do
			resp <- http req' (httpManager uo)
			-- forces processing the response while
			-- within the runResourceT
			liftIO $ if responseStatus resp == ok200
				then found
					(extractlen resp)
					(extractfilename resp)
				else dne

	existscurl u = do
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
			_ -> dne

-- Parse eg: attachment; filename="fname.ext"
-- per RFC 2616
contentDispositionFilename :: String -> Maybe FilePath
contentDispositionFilename s
	| "attachment; filename=\"" `isPrefixOf` s && "\"" `isSuffixOf` s =
		Just $ reverse $ drop 1 $ reverse $ 
			drop 1 $ dropWhile (/= '"') s
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
 - By default, conduit is used for the download, except for file: urls,
 - which are copied. If the url scheme is not supported by conduit, falls
 - back to using curl.
 -
 - Displays error message on stderr when download failed.
 -}
download :: MeterUpdate -> URLString -> FilePath -> UrlOptions -> IO Bool
download meterupdate url file uo =
	catchJust matchHttpException go showhttpexception
		`catchNonAsync` showerr
  where
	go = case parseURIRelaxed url of
		Just u -> case (urlDownloader uo, parseUrlConduit (show u)) of
			(DownloadWithConduit, Just req) -> catchJust
				-- When http redirects to a protocol which 
				-- conduit does not support, it will throw
				-- a StatusCodeException with found302.
				(matchStatusCodeException (== found302))
				(downloadconduit req)
				(const downloadcurl)
			_
				| uriScheme u == "file:" -> do
					let src = unEscapeString (uriPath u)
					withMeteredFile src meterupdate $
						L.writeFile file
					return True
				| otherwise -> downloadcurl
		Nothing -> return False

	downloadconduit req = catchMaybeIO (getFileSize file) >>= \case
		Nothing -> runResourceT $ do
			resp <- http req (httpManager uo)
			if responseStatus resp == ok200
				then store zeroBytesProcessed WriteMode resp
				else showrespfailure resp
		Just sz -> resumeconduit req sz
	
	alreadydownloaded sz s h = s == requestedRangeNotSatisfiable416 
		&& case lookup hContentRange h of
			-- This could be improved by fixing
			-- https://github.com/aristidb/http-types/issues/87
			Just crh -> crh == B8.fromString ("bytes */" ++ show sz)
			Nothing -> False

	-- Resume download from where a previous download was interrupted, 
	-- when supported by the http server. The server may also opt to
	-- send the whole file rather than resuming.
	resumeconduit req sz = catchJust
		(matchStatusCodeHeadersException (alreadydownloaded sz))
		dl
		(const $ return True)
	  where
		dl = runResourceT $ do
			let req' = req { requestHeaders = resumeFromHeader sz : requestHeaders req }
			resp <- http req' (httpManager uo)
			if responseStatus resp == partialContent206
				then store (BytesProcessed sz) AppendMode resp
				else if responseStatus resp == ok200
					then store zeroBytesProcessed WriteMode resp
					else showrespfailure resp
	
	showrespfailure resp = liftIO $ do
		hPutStrLn stderr $ B8.toString $
			statusMessage $ responseStatus resp
		hFlush stderr
		return False
	showhttpexception he = do
#if MIN_VERSION_http_client(0,5,0)
		let msg = case he of
			HttpExceptionRequest _ (StatusCodeException _ msgb) ->
				B8.toString msgb
			HttpExceptionRequest _ other -> show other
			_ -> show he
#else
		let msg = case he of
			StatusCodeException status _ _ -> 
				B8.toString (statusMessage status)
			_ -> show he
#endif
		hPutStrLn stderr $ "download failed: " ++ msg
		hFlush stderr
		return False
	showerr e = do
		hPutStrLn stderr (show e)
		hFlush stderr
		return False
	
	store initialp mode resp = do
		sinkResponseFile meterupdate initialp file mode resp
		return True
	
	downloadcurl = do
		-- curl does not create destination file
		-- if the url happens to be empty, so pre-create.
		unlessM (doesFileExist file) $
			writeFile file ""
		let ps = curlParams uo
			[ Param "-sS"
			, Param "-f"
			, Param "-L"
			, Param "-C", Param "-"
			]
		boolSystem "curl" (ps ++ [Param "-o", File file, File url])

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
#if MIN_VERSION_http_conduit(2,3,0)
	-> Response (ConduitM () B8.ByteString m ())
#else
	-> Response (ResumableSource m B8.ByteString)
#endif
	-> m ()
sinkResponseFile meterupdate initialp file mode resp = do
	(fr, fh) <- allocate (openBinaryFile file mode) hClose
#if MIN_VERSION_http_conduit(2,3,0)
	runConduit $ responseBody resp .| go initialp fh
#else
	responseBody resp $$+- go initialp fh
#endif
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
	go u = case parseUrlConduit (show u) of
		Nothing -> return Nothing
		Just req -> do
			let req' = applyRequest uo req
			withResponse req' (httpManager uo) $ \resp ->
				if responseStatus resp == ok200
					then Just <$> brread n [] (responseBody resp)
					else return Nothing

	-- could use brReadSome here, needs newer http-client dependency
	brread n' l rb
		| n' <= 0 = return (L.fromChunks (reverse l))
		| otherwise = do
			bs <- brRead rb
			if B.null bs
				then return (L.fromChunks (reverse l))
				else brread (n' - B.length bs) (bs:l) rb

{- Allows for spaces and other stuff in urls, properly escaping them. -}
parseURIRelaxed :: URLString -> Maybe URI
parseURIRelaxed s = maybe (parseURIRelaxed' s) Just $
	parseURI $ escapeURIString isAllowedInURI s

parseUrlConduit :: URLString -> Maybe Request
#if MIN_VERSION_http_client(0,4,30)
parseUrlConduit = parseUrlThrow
#else
parseUrlConduit = parseUrl
#endif

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

#if MIN_VERSION_http_client(0,5,0)
matchStatusCodeHeadersException :: (Status -> ResponseHeaders -> Bool) -> HttpException -> Maybe HttpException
matchStatusCodeHeadersException want e@(HttpExceptionRequest _ (StatusCodeException r _))
	| want (responseStatus r) (responseHeaders r) = Just e
	| otherwise = Nothing
matchStatusCodeHeadersException _ _ = Nothing
#else
matchStatusCodeHeadersException :: (Status -> ResponseHeaders -> Bool) -> HttpException -> Maybe HttpException
matchStatusCodeHeadersException want e@(StatusCodeException s r _)
	| want s r = Just e
	| otherwise = Nothing
matchStatusCodeHeadersException _ _ = Nothing
#endif

{- Use with eg: 
 -
 - > catchJust matchHttpException
 -}
matchHttpException :: HttpException -> Maybe HttpException
matchHttpException = Just

#if MIN_VERSION_http_client(0,5,0)
matchHttpExceptionContent :: (HttpExceptionContent -> Bool) -> HttpException -> Maybe HttpException
matchHttpExceptionContent want e@(HttpExceptionRequest _ hec)
	| want hec = Just e
	| otherwise = Nothing
matchHttpExceptionContent _ _ = Nothing
#else
matchHttpExceptionContent :: (HttpException -> Bool) -> HttpException -> Maybe HttpException
matchHttpExceptionContent want e
	| want e = Just e
	| otherwise = Nothing
#endif

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
import Network.URI

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
exists url headers = case parseURIRelaxed url of
	Just u
		| uriScheme u == "file:" -> do
			s <- catchMaybeIO $ getFileStatus (unEscapeString $ uriPath u)
			case s of
				Just stat -> return (True, Just $ fromIntegral $ fileSize stat)
				Nothing -> dne
		| otherwise -> do
			output <- readProcess "curl" curlparams
			case lastMaybe (lines output) of
				Just ('2':_:_) -> return (True, extractsize output)
				_ -> dne
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

{- Used to download large files, such as the contents of keys.
 -
 - Uses wget or curl program for its progress bar. (Wget has a better one,
 - so is preferred.) Which program to use is determined at run time; it
 - would not be appropriate to test at configure time and build support
 - for only one in.
 -}
download :: URLString -> Headers -> [CommandParam] -> FilePath -> IO Bool
download url headers options file = 
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
get url headers = readProcess "curl" $
	["-s", "-L", url] ++ concatMap (\h -> ["-H", h]) headers

{- Allows for spaces and other stuff in urls, properly escaping them. -}
parseURIRelaxed :: URLString -> Maybe URI
parseURIRelaxed = parseURI . escapeURIString isAllowedInURI

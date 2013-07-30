{- git-annex "URL" backend -- keys whose content is available from urls.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.URL (
	backends,
	fromUrl
) where

import Data.Hash.MD5

import Common.Annex
import Types.Backend
import Types.Key

backends :: [Backend]
backends = [backend]

backend :: Backend
backend = Backend
	{ name = "URL"
	, getKey = const $ return Nothing
	, fsckKey = Nothing
	, canUpgradeKey = Nothing
	}

{- When it's not too long, use the full url as the key name.
 - If the url is too long, it's truncated at half the filename length
 - limit, and the md5 of the url is prepended to ensure a unique key. -}
fromUrl :: String -> Maybe Integer -> Annex Key
fromUrl url size = do
	limit <- liftIO . fileNameLengthLimit =<< fromRepo gitAnnexDir
	let truncurl = truncateFilePath (limit `div` 2) url
	let key = if url == truncurl
		then url
		else truncurl ++ "-" ++ md5s (Str url)
	return $ stubKey
		{ keyName = key
		, keyBackendName = "URL"
		, keySize = size
	}

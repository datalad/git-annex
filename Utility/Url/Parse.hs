{- Url parsing.
 -
 - Copyright 2011-2023 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Utility.Url.Parse (
	parseURIPortable,
	parseURIRelaxed,
) where

import Network.URI
#ifdef mingw32_HOST_OS
import qualified System.FilePath.Windows as PW
#endif

{- On unix this is the same as parseURI. But on Windows,
 - it can parse urls such as file:///C:/path/to/file
 - parseURI normally parses that as a path /C:/path/to/file
 - and this simply removes the excess leading slash when there is a
 - drive letter after it. -}
parseURIPortable :: String -> Maybe URI
#ifndef mingw32_HOST_OS
parseURIPortable = parseURI
#else
parseURIPortable s
	| "file:" `isPrefixOf` s = do
		u <- parseURI s
		return $ case PW.splitDirectories (uriPath u) of
			(p:d:_) | all PW.isPathSeparator p && PW.isDrive d ->
				u { uriPath = dropWhile PW.isPathSeparator (uriPath u) }
			_ -> u
	| otherwise = parseURI s
#endif

{- Allows for spaces and other stuff in urls, properly escaping them. -}
parseURIRelaxed :: String -> Maybe URI
parseURIRelaxed s = maybe (parseURIRelaxed' s) Just $
	parseURIPortable $ escapeURIString isAllowedInURI s

{- Some characters like '[' are allowed in eg, the address of
 - an uri, but cannot appear unescaped further along in the uri.
 - This handles that, expensively, by successively escaping each character
 - from the back of the url until the url parses.
 -}
parseURIRelaxed' :: String -> Maybe URI
parseURIRelaxed' s = go [] (reverse s)
  where
	go back [] = parseURI back
	go back (c:cs) = case parseURI (escapeURIString isAllowedInURI (reverse (c:cs)) ++ back) of
		Just u -> Just u
		Nothing -> go (escapeURIChar escapemore c ++ back) cs

	escapemore '[' = False
	escapemore ']' = False
	escapemore c = isAllowedInURI c

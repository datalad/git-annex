{- git-config http.<url>.* handling
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.Config.Url (
	getHttpConfig,
	httpConfigKeys,
	prop_httpConfigKeys_sane,
) where

import qualified Data.Map as M
import Network.URI
import Data.Function

import Common
import Git
import Git.Types
import Utility.Glob

{- Gets any per-url settings from the git config for a http.foo ConfigKey.
 -
 - If there are non, falls back to the non-url-spcecific config, if any,
 - or the provided fallback value.
 -
 - See git-config(1)'s documentation of http.<url>.* for the details.
 -}
getHttpConfig :: ConfigKey -> ConfigValue -> URI -> Repo -> ConfigValue
getHttpConfig key fallback url repo = fromMaybe fallback $
	case httpConfigKeys key url (config repo) of
		[] -> Nothing
		(k:_) -> M.lookup k (config repo)

{- Gets any per-url config keys for a non-url-specific input
 - http.foo ConfigKey that match the provided url.
 -
 - The list is ordered by decreasing precedance and includes the input
 - ConfigKey at the end when it's part of the RepoConfig.
 -}
httpConfigKeys :: ConfigKey -> URI -> RepoConfig -> [ConfigKey]
httpConfigKeys nonurlspecifickey@(ConfigKey key) urltomatch c =
	let l = map fst 
		$ reverse $ sortBy precedence 
		$ mapMaybe matching (M.keys c)
	in if M.member nonurlspecifickey c
		then l ++ [nonurlspecifickey]
		else l
  where
	httpprefix = "http."
	keysuffix = case decodeBS key of
		('h':'t':'t':'p':'.':rest) -> '.' : rest
		v -> v
	httpprefixlen = length httpprefix
	keysuffixlen = length keysuffix

	extracturlfromkey = parseURI 
		. reverse . drop keysuffixlen . reverse 
		. drop httpprefixlen

	precedence (_k1, u1) (_k2, u2) =
		(compare `on` (length . uriPath)) u1 u2
			<> (compare `on` (uriUserInfo <$$> uriAuthority)) u1 u2
	
	matching k@(ConfigKey ck) = 
		let sk = decodeBS ck
		in if httpprefix `isPrefixOf` sk && keysuffix `isSuffixOf` sk && k /= nonurlspecifickey
			then do
				u <- extracturlfromkey sk
				let same f = f u == f urltomatch
				if same uriScheme
					&& (same (uriRegName <$$> uriAuthority)
						|| subdomainwildcardmatch u)
					&& same getportordefault
					&& (same uriPath
						|| pathslashprefix u)
					&& (same getusername
						|| getusername u == Nothing)
					then Just (k, u)
					else Nothing
			else Nothing
	
	getportordefault u = do
		a <- uriAuthority u
		if null (uriPort a)
			then case uriScheme u of
				"http:" -> return ":80"
				"https:" -> return ":443"
				_ -> Nothing
			else return (uriPort a)

	getusername u = do
		a <- uriAuthority u
		let (user, _pass) = break (== ':') (uriUserInfo a)
		let username = fst (break (== '@') user)
		if null username
			then Nothing
			else return username

	pathslashprefix u = 
		let p = if "/" `isSuffixOf` uriPath u
			then uriPath u
			else uriPath u ++ "/"
		in p `isPrefixOf` uriPath urltomatch

	subdomainwildcardmatch u =
		subdomainwildcardmatch' (uridomains u) (uridomains urltomatch)
	
	subdomainwildcardmatch' [] [] = True
	subdomainwildcardmatch' [] _ = False
	subdomainwildcardmatch' _ [] = False
	subdomainwildcardmatch' (a:as) (b:bs)
		| a == b = subdomainwildcardmatch' as bs
		| otherwise =
			let g = compileGlob a CaseInsensitive (GlobFilePath False)
			in if matchGlob g b
				then subdomainwildcardmatch' as bs
				else False

	uridomains u = case uriRegName <$> uriAuthority u of
		Nothing -> []
		Just d -> splitc '.' d

prop_httpConfigKeys_sane :: Bool
prop_httpConfigKeys_sane = and prop_httpConfigKeys_tests

prop_httpConfigKeys_tests :: [Bool]
prop_httpConfigKeys_tests =
	[ httpConfigKeys (ConfigKey "http.foo") u c ==
		[ "http.http://user@example.com/foo/bar.foo"
		, "http.http://example.com/foo/bar.foo"
		, "http.http://example.com/foo.foo"
		, "http.http://example.com.foo"
		, "http.foo"
		]
	, httpConfigKeys (ConfigKey "http.bar") u c ==
		[ "http.http://*.com.bar" ]
	, httpConfigKeys (ConfigKey "http.baz") u c ==
		[ "http.http://example.com:80.baz" ]
	, httpConfigKeys (ConfigKey "http.baz") uwithport c ==
		[ "http.http://example.com:8080.baz" ]
	]
  where
	u = fromMaybe (error "internal") $ 
		parseURI "http://user:password@example.com/foo/bar/"
	uwithport = fromMaybe (error "internal") $ 
		parseURI "http://user:password@example.com:8080/foo/bar/"
	c = M.fromList $ map (\k -> (ConfigKey k, ConfigValue "dummy value"))
		[ "http.foo"
		, "http.http://example.co.foo"
		, "http.http://example.com.foo"
		, "https.http://example.com.foo"
		, "http.http://example.com/fo.foo"
		, "http.http://example.com/foo.foo"
		, "http.http://example.com/foo/ba.foo"
		, "http.http://example.com/foo/bar.foo"
		, "http.http://user@example.com/foo/bar.foo"
		, "http.http://nonmatchingexample.com.foo"
		, "http.http://*.bar"
		, "http.http://*.com.bar"
		, "http.http://example.com:80.baz"
		, "http.http://example.com:443.baz"
		, "http.http://example.com:8080.baz"
		]

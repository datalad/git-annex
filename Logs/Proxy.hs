{- git-annex proxy log
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Logs.Proxy (
	Proxy(..),
	getProxies,
	recordProxies,
) where

import qualified Annex
import Annex.Common
import qualified Annex.Branch
import qualified Git.Remote
import Logs
import Logs.UUIDBased
import Logs.MapLog
import Annex.UUID

import qualified Data.Set as S
import qualified Data.Map as M
import Data.ByteString.Builder
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString.Lazy as L

data Proxy = Proxy
	{ proxyRemoteUUID :: UUID
	, proxyRemoteName :: RemoteName
	} deriving (Show, Eq, Ord)

getProxies :: Annex (M.Map UUID (S.Set Proxy))
getProxies = M.map (validateProxies . value) . fromMapLog . parseProxyLog
	<$> Annex.Branch.get proxyLog

recordProxies :: S.Set Proxy -> Annex ()
recordProxies proxies = do
	-- If a private UUID has been configured as a proxy, avoid leaking
	-- it into the git-annex log.
	privateuuids <- annexPrivateRepos <$> Annex.getGitConfig
	let proxies' = S.filter
		(\p -> S.notMember (proxyRemoteUUID p) privateuuids) proxies
	
	c <- currentVectorClock
	u <- getUUID
	Annex.Branch.change (Annex.Branch.RegardingUUID [u]) proxyLog $
		(buildLogNew buildProxyList)
			. changeLog c u proxies'
			. parseProxyLog

buildProxyList :: S.Set Proxy -> Builder
buildProxyList = assemble . map fmt . S.toList
  where
	fmt p = buildUUID (proxyRemoteUUID p)
		<> colon
		<> byteString (encodeBS (proxyRemoteName p))
	colon = charUtf8 ':'
	
	assemble [] = mempty
	assemble (x:[]) = x
	assemble (x:y:l) = x <> " " <> assemble (y:l)

parseProxyLog :: L.ByteString -> Log (S.Set Proxy)
parseProxyLog = parseLogNew parseProxyList

parseProxyList :: A.Parser (S.Set Proxy)
parseProxyList = S.fromList <$> many parseword
  where
	parseword = parseproxy
		<* ((const () <$> A8.char ' ') <|> A.endOfInput)
	parseproxy = Proxy
		<$> (toUUID <$> A8.takeWhile1 (/= colon))
		<* (const () <$> A8.char colon)
		<*> (decodeBS <$> A8.takeWhile1 (/= ' '))
	colon = ':'

-- Filter out any proxies that have a name that is not allowed as a git
-- remote name. This avoids any security problems with eg escape
-- characters in names, and ensures the name can be used anywhere a usual
-- git remote name can be used without causing issues.
validateProxies :: S.Set Proxy -> S.Set Proxy
validateProxies = S.filter $ Git.Remote.isLegalName . proxyRemoteName

{- git-annex trust log, pure operations
 -
 - Copyright 2010-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Logs.Trust.Pure where

import Annex.Common
import Types.TrustLevel
import Logs.UUIDBased

import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString.Builder

calcTrustMap :: L.ByteString -> TrustMap
calcTrustMap = simpleMap . parseLogOld trustLevelParser

trustLevelParser :: A.Parser TrustLevel
trustLevelParser = (totrust <$> A8.anyChar <* A.endOfInput)
	-- The trust log used to only list trusted repos, without a 
	-- value for the trust status
	<|> (const Trusted <$> A.endOfInput)
  where
	totrust '1' = Trusted
	totrust '0' = UnTrusted
	totrust 'X' = DeadTrusted
	-- Allow for future expansion by treating unknown trust levels as
	-- semitrusted.
	totrust _ = SemiTrusted

buildTrustLevel :: TrustLevel -> Builder
buildTrustLevel Trusted = byteString "1"
buildTrustLevel UnTrusted = byteString "0"
buildTrustLevel DeadTrusted = byteString "X"
buildTrustLevel SemiTrusted = byteString "?"

prop_parse_build_TrustLevelLog :: Bool
prop_parse_build_TrustLevelLog = all check [minBound .. maxBound]
  where
	check l = 
		let v = A.parseOnly trustLevelParser $ L.toStrict $
			toLazyByteString $ buildTrustLevel l
		in v == Right l

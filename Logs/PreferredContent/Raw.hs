{- unparsed preferred content expressions
 -
 - Copyright 2012-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.PreferredContent.Raw where

import Annex.Common
import qualified Annex.Branch
import qualified Annex
import Logs
import Logs.UUIDBased
import Logs.MapLog
import Types.StandardGroups
import Types.Group

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.ByteString.Builder

{- Changes the preferred content configuration of a remote. -}
preferredContentSet :: UUID -> PreferredContentExpression -> Annex ()
preferredContentSet = setLog preferredContentLog

requiredContentSet :: UUID -> PreferredContentExpression -> Annex ()
requiredContentSet = setLog requiredContentLog

setLog :: RawFilePath -> UUID -> PreferredContentExpression -> Annex ()
setLog logfile uuid@(UUID _) val = do
	c <- currentVectorClock
	Annex.Branch.change logfile $
		buildLogOld buildPreferredContentExpression
		. changeLog c uuid val
		. parseLogOld parsePreferredContentExpression
	Annex.changeState $ \s -> s 
		{ Annex.preferredcontentmap = Nothing
		, Annex.requiredcontentmap = Nothing
		}
setLog _ NoUUID _ = error "unknown UUID; cannot modify"

{- Changes the preferred content configuration of a group. -}
groupPreferredContentSet :: Group -> PreferredContentExpression -> Annex ()
groupPreferredContentSet g val = do
	c <- currentVectorClock
	Annex.Branch.change groupPreferredContentLog $
		buildGroupPreferredContent
		. changeMapLog c g val 
		. parseGroupPreferredContent
	Annex.changeState $ \s -> s { Annex.preferredcontentmap = Nothing }

parseGroupPreferredContent :: L.ByteString -> MapLog Group String
parseGroupPreferredContent = parseMapLog parsegroup parsestring
  where
	parsegroup = Group <$> A.takeByteString
	parsestring = decodeBS <$> A.takeByteString

buildGroupPreferredContent :: MapLog Group PreferredContentExpression -> Builder
buildGroupPreferredContent = buildMapLog buildgroup buildexpr
  where
	buildgroup (Group g) = byteString g
	buildexpr = byteString . encodeBS

parsePreferredContentExpression :: A.Parser PreferredContentExpression
parsePreferredContentExpression = decodeBS <$> A.takeByteString

buildPreferredContentExpression :: PreferredContentExpression -> Builder
buildPreferredContentExpression = byteString . encodeBS

preferredContentMapRaw :: Annex (M.Map UUID PreferredContentExpression)
preferredContentMapRaw = simpleMap . parseLogOld parsePreferredContentExpression
	<$> Annex.Branch.get preferredContentLog

requiredContentMapRaw :: Annex (M.Map UUID PreferredContentExpression)
requiredContentMapRaw = simpleMap . parseLogOld parsePreferredContentExpression
	<$> Annex.Branch.get requiredContentLog

groupPreferredContentMapRaw :: Annex (M.Map Group PreferredContentExpression)
groupPreferredContentMapRaw = simpleMap . parseGroupPreferredContent
	<$> Annex.Branch.get groupPreferredContentLog

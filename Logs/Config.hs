{- git-annex repository-global config log
 -
 - Copyright 2017-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Config (
	ConfigName,
	ConfigValue,
	setGlobalConfig,
	unsetGlobalConfig,
	getGlobalConfig,
	loadGlobalConfig,
) where

import Annex.Common
import Logs
import Logs.MapLog
import qualified Annex.Branch

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.ByteString.Builder

type ConfigName = String
type ConfigValue = String

setGlobalConfig :: ConfigName -> ConfigValue -> Annex ()
setGlobalConfig name new = do
	curr <- getGlobalConfig name
	when (curr /= Just new) $
		setGlobalConfig' name new

setGlobalConfig' :: ConfigName -> ConfigValue -> Annex ()
setGlobalConfig' name new = do
	c <- liftIO currentVectorClock
	Annex.Branch.change configLog $ 
		buildGlobalConfig . changeMapLog c name new . parseGlobalConfig

unsetGlobalConfig :: ConfigName -> Annex ()
unsetGlobalConfig name = do
	curr <- getGlobalConfig name
	when (curr /= Nothing) $
		setGlobalConfig' name "" -- set to empty string to unset

-- Reads the global config log every time.
getGlobalConfig :: ConfigName -> Annex (Maybe ConfigValue)
getGlobalConfig name = M.lookup name <$> loadGlobalConfig

buildGlobalConfig :: MapLog ConfigName ConfigValue -> Builder
buildGlobalConfig = buildMapLog fieldbuilder valuebuilder
  where
	fieldbuilder = byteString . encodeBS
	valuebuilder = byteString . encodeBS

parseGlobalConfig :: L.ByteString -> MapLog ConfigName ConfigValue
parseGlobalConfig = parseMapLog string string
  where
	string = decodeBS <$> A.takeByteString

loadGlobalConfig :: Annex (M.Map ConfigName ConfigValue)
loadGlobalConfig = M.filter (not . null) . simpleMap . parseGlobalConfig
	<$> Annex.Branch.get configLog

{- git-annex repository-global config log
 -
 - Copyright 2017-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
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
		buildGlobalConfig . changeMapLog c name new . parseGlobalConfig . decodeBL

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

parseGlobalConfig :: String -> MapLog ConfigName ConfigValue
parseGlobalConfig = parseMapLog Just Just

loadGlobalConfig :: Annex (M.Map ConfigName ConfigValue)
loadGlobalConfig = M.filter (not . null) . simpleMap . parseGlobalConfig . decodeBL
	<$> Annex.Branch.get configLog

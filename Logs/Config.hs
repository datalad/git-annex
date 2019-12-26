{- git-annex repository-global config log
 -
 - Copyright 2017-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Config (
	ConfigKey(..),
	ConfigValue(..),
	setGlobalConfig,
	unsetGlobalConfig,
	getGlobalConfig,
	loadGlobalConfig,
) where

import Annex.Common
import Logs
import Logs.MapLog
import qualified Annex.Branch
import Git.Types (ConfigKey(..), ConfigValue(..))

import qualified Data.Map as M
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.ByteString.Builder

setGlobalConfig :: ConfigKey -> ConfigValue -> Annex ()
setGlobalConfig name new = do
	curr <- getGlobalConfig name
	when (curr /= Just new) $
		setGlobalConfig' name new

setGlobalConfig' :: ConfigKey -> ConfigValue -> Annex ()
setGlobalConfig' name new = do
	c <- liftIO currentVectorClock
	Annex.Branch.change configLog $ 
		buildGlobalConfig . changeMapLog c name new . parseGlobalConfig

unsetGlobalConfig :: ConfigKey -> Annex ()
unsetGlobalConfig name = do
	curr <- getGlobalConfig name
	when (curr /= Nothing) $
		-- set to empty string to unset
		setGlobalConfig' name (ConfigValue mempty)

-- Reads the global config log every time.
-- It's more efficient to use Config.GitConfig.
getGlobalConfig :: ConfigKey -> Annex (Maybe ConfigValue)
getGlobalConfig name = M.lookup name <$> loadGlobalConfig

buildGlobalConfig :: MapLog ConfigKey ConfigValue -> Builder
buildGlobalConfig = buildMapLog configkeybuilder valuebuilder
  where
	configkeybuilder (ConfigKey k) = byteString k
	valuebuilder (ConfigValue v) = byteString v

parseGlobalConfig :: L.ByteString -> MapLog ConfigKey ConfigValue
parseGlobalConfig = parseMapLog configkeyparser valueparser
  where
	configkeyparser = ConfigKey <$> A.takeByteString
	valueparser = ConfigValue <$> A.takeByteString

loadGlobalConfig :: Annex (M.Map ConfigKey ConfigValue)
loadGlobalConfig = M.filter (\(ConfigValue v) -> not (S.null v)) 
	. simpleMap
	. parseGlobalConfig
	<$> Annex.Branch.get configLog

{- git-annex repository-global config log
 -
 - Copyright 2017-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Config (
	ConfigKey,
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
import Git.Types (ConfigKey(..))

import qualified Data.Map as M
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.ByteString.Builder

type ConfigValue = S.ByteString

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
		setGlobalConfig' name mempty -- set to empty string to unset

-- Reads the global config log every time.
getGlobalConfig :: ConfigKey -> Annex (Maybe ConfigValue)
getGlobalConfig name = M.lookup name <$> loadGlobalConfig

buildGlobalConfig :: MapLog ConfigKey ConfigValue -> Builder
buildGlobalConfig = buildMapLog configkeybuilder valuebuilder
  where
	configkeybuilder (ConfigKey f) = byteString f
	valuebuilder = byteString

parseGlobalConfig :: L.ByteString -> MapLog ConfigKey ConfigValue
parseGlobalConfig = parseMapLog configkeyparser valueparser
  where
	configkeyparser = ConfigKey <$> A.takeByteString
	valueparser = A.takeByteString

loadGlobalConfig :: Annex (M.Map ConfigKey ConfigValue)
loadGlobalConfig = M.filter (not . S.null) . simpleMap . parseGlobalConfig
	<$> Annex.Branch.get configLog

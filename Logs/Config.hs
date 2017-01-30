{- git-annex config log
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
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
import qualified Annex
import Logs
import Logs.MapLog
import qualified Annex.Branch

import Data.Time.Clock.POSIX
import qualified Data.Map as M

type ConfigName = String
type ConfigValue = String

setGlobalConfig :: ConfigName -> ConfigValue -> Annex ()
setGlobalConfig name new = do
	curr <- getGlobalConfig name
	when (curr /= Just new) $
		setGlobalConfig' name new

setGlobalConfig' :: ConfigName -> ConfigValue -> Annex ()
setGlobalConfig' name new = do
	now <- liftIO getPOSIXTime
	Annex.Branch.change configLog $ 
		showMapLog id id . changeMapLog now name new . parseGlobalConfig

unsetGlobalConfig :: ConfigName -> Annex ()
unsetGlobalConfig name = do
	curr <- getGlobalConfig name
	when (curr /= Nothing) $
		setGlobalConfig' name "" -- set to empty string to unset

getGlobalConfig :: ConfigName -> Annex (Maybe ConfigValue)
getGlobalConfig name = do
	m <- maybe loadGlobalConfig return
		=<< Annex.getState Annex.globalconfig
	return (M.lookup name m)

parseGlobalConfig :: String -> MapLog ConfigName ConfigValue
parseGlobalConfig = parseMapLog Just Just

loadGlobalConfig :: Annex (M.Map ConfigName ConfigValue)
loadGlobalConfig = do
	m <- M.filter (not . null) . simpleMap . parseGlobalConfig
		<$> Annex.Branch.get configLog
	Annex.changeState $ \s -> s { Annex.globalconfig = Just m }
	return m

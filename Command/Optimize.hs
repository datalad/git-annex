{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Optimize where

import Command
import Utility
import LocationLog
import Trust
import Config
import qualified Command.Get
import qualified Command.Drop

command :: [Command]
command = [repoCommand "optimize" (paramOptional $ paramRepeating paramPath) seek
	"get or drop content to best use available space"]

seek :: [CommandSeek]
seek = [withNumCopies start]

start :: CommandStartAttrFile
start p@(file, attr) = notBareRepo $ isAnnexed file $ \(key, _) -> do
	needed <- getNumCopies numcopies
	(_, safelocations) <- trustPartition UnTrusted =<< keyLocations key
	dispatch needed (length safelocations)
	where
		dispatch needed present
			| present < needed = Command.Get.start file
			| present > needed = Command.Drop.start p
			| otherwise = stop
		numcopies = readMaybe attr :: Maybe Int

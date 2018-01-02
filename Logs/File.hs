{- git-annex log files
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.File where

import Annex.Common
import Annex.Perms
import Utility.Tmp

writeLogFile :: FilePath -> String -> Annex ()
writeLogFile f c = go `catchNonAsync` \_e -> do
	-- Most of the time, the directory will exist, so this is only
	-- done if writing the file fails.
	createAnnexDirectory (parentDir f)
	go
  where
	go = viaTmp writelog f c
	writelog f' c' = do
		liftIO $ writeFile f' c'
		setAnnexFilePerm f'

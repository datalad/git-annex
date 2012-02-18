{- Web url logs.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Web (
	URLString,
	webUUID,
	setUrl,
	setUrlPresent,
	getUrls
) where

import Common.Annex
import Logs.Presence
import Logs.Location

type URLString = String

-- Dummy uuid for the whole web. Do not alter.
webUUID :: UUID
webUUID = UUID "00000000-0000-0000-0000-000000000001"

urlLog :: Key -> FilePath
urlLog key = hashDirLower key </> keyFile key ++ ".log.web"

{- Used to store the urls elsewhere. -}
oldurlLogs :: Key -> [FilePath]
oldurlLogs key = 
	[ "remote/web" </> hashDirLower key </> show key ++ ".log"
	, "remote/web" </> hashDirLower key </> keyFile key ++ ".log"
	]

{- Gets all urls that a key might be available from. -}
getUrls :: Key -> Annex [URLString]
getUrls key = go $ urlLog key : oldurlLogs key
	where
		go [] = return []
		go (l:ls) = do
			us <- currentLog l
			if null us
				then go ls
				else return us

{- Records a change in an url for a key. -}
setUrl :: Key -> URLString -> LogStatus -> Annex ()
setUrl key url status = do
	addLog (urlLog key) =<< logNow status url

	-- update location log to indicate that the web has the key, or not
	us <- getUrls key
	logChange key webUUID (if null us then InfoMissing else InfoPresent)

setUrlPresent :: Key -> URLString -> Annex ()
setUrlPresent key url = setUrl key url InfoPresent

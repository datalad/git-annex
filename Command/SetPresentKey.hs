{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.SetPresentKey where

import Command
import Logs.Location
import Logs.Presence.Pure

cmd :: Command
cmd = noCommit $ withAnnexOptions [jsonOptions] $
	command "setpresentkey" SectionPlumbing
		"change records of where key is present"
		(paramPair paramKey (paramPair paramUUID "[1|0]"))
		(seek <$$> optParser)

data SetPresentKeyOptions = SetPresentKeyOptions
	{ params :: CmdParams
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser SetPresentKeyOptions
optParser desc = SetPresentKeyOptions
	<$> cmdParams desc
	<*> parseBatchOption False

seek :: SetPresentKeyOptions -> CommandSeek
seek o = case batchOption o of
	Batch fmt -> batchOnly Nothing (params o) $
		batchInput fmt
			(pure . parseKeyStatus . words)
			(batchCommandAction . uncurry start)
	NoBatch -> either giveup (commandAction . start (SeekInput (params o)))
		(parseKeyStatus $ params o)

data KeyStatus = KeyStatus Key UUID LogStatus

parseKeyStatus :: [String] -> Either String KeyStatus
parseKeyStatus (ks:us:vs:[]) = do
	k <- maybe (Left "bad key") Right (deserializeKey ks)
	let u = toUUID us
	s <- maybe (Left "bad value") Right (parseStatus vs)
	return $ KeyStatus k u s
parseKeyStatus _ = Left "Bad input. Expected: key uuid value"

start :: SeekInput -> KeyStatus -> CommandStart
start si (KeyStatus k u s) = starting "setpresentkey" ai si $ perform k u s
  where
	ai = mkActionItem k

perform :: Key -> UUID -> LogStatus -> CommandPerform
perform k u s = next $ do
	logChange NoLiveUpdate k u s
	return True

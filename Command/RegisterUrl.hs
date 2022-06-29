{- git-annex command
 -
 - Copyright 2015-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.RegisterUrl where

import Command
import Logs.Web
import Command.FromKey (keyOpt, keyOpt')
import qualified Remote

cmd :: Command
cmd = withAnnexOptions [jsonOptions] $ command "registerurl"
	SectionPlumbing "registers an url for a key"
	(paramPair paramKey paramUrl)
	(seek <$$> optParser)

data RegisterUrlOptions = RegisterUrlOptions
	{ keyUrlPairs :: CmdParams
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser RegisterUrlOptions
optParser desc = RegisterUrlOptions
	<$> cmdParams desc
	<*> parseBatchOption False

seek :: RegisterUrlOptions -> CommandSeek
seek o = case (batchOption o, keyUrlPairs o) of
	(Batch fmt, _) -> seekBatch setUrlPresent o fmt
	-- older way of enabling batch input, does not support BatchNull
	(NoBatch, []) -> seekBatch setUrlPresent o (BatchFormat BatchLine (BatchKeys False))
	(NoBatch, ps) -> commandAction (start setUrlPresent ps)

seekBatch :: (Key -> URLString -> Annex ()) -> RegisterUrlOptions -> BatchFormat -> CommandSeek
seekBatch a o fmt = batchOnly Nothing (keyUrlPairs o) $
	batchInput fmt (pure . parsebatch) $
		batchCommandAction . start' a
  where
	parsebatch l = 
		let (keyname, u) = separate (== ' ') l
		in if null u
			then Left "no url provided"
			else case keyOpt' keyname of
				Left e -> Left e
				Right k -> Right (k, u)

start :: (Key -> URLString -> Annex ()) -> [String] -> CommandStart
start a (keyname:url:[]) = start' a (si, (keyOpt keyname, url))
  where
	si = SeekInput [keyname, url]
start _ _ = giveup "specify a key and an url"

start' :: (Key -> URLString -> Annex ()) -> (SeekInput, (Key, URLString)) -> CommandStart
start' a (si, (key, url)) =
	starting "registerurl" ai si $
		perform a key url
  where
	ai = ActionItemOther (Just url)

perform :: (Key -> URLString -> Annex ()) -> Key -> URLString -> CommandPerform
perform a key url = do
	r <- Remote.claimingUrl url
	a key (setDownloader' url r)
	next $ return True

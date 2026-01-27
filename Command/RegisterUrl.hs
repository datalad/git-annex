{- git-annex command
 -
 - Copyright 2015-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.RegisterUrl where

import Command
import Logs.Web
import Logs.Location
import Command.FromKey (keyOpt, keyOpt')
import qualified Remote
import Annex.UUID

cmd :: Command
cmd = withAnnexOptions [jsonOptions] $ command "registerurl"
	SectionPlumbing "registers an url for a key"
	(paramPair paramKey paramUrl)
	(seek <$$> optParser)

data RegisterUrlOptions = RegisterUrlOptions
	{ keyUrlPairs :: CmdParams
	, batchOption :: BatchMode
	, remoteOption :: Maybe (DeferredParse Remote)
	}

optParser :: CmdParamsDesc -> Parser RegisterUrlOptions
optParser desc = RegisterUrlOptions
	<$> cmdParams desc
	<*> parseBatchOption False
	<*> optional (mkParseRemoteOption <$> parseRemoteOption)

seek :: RegisterUrlOptions -> CommandSeek
seek o = case (batchOption o, keyUrlPairs o) of
	(Batch fmt, _) -> seekBatch registerUrl o fmt
	-- older way of enabling batch input, does not support BatchNull
	(NoBatch, []) -> seekBatch registerUrl o (BatchFormat BatchLine (BatchKeys False))
	(NoBatch, ps) -> commandAction (start "registerurl" registerUrl o ps)

seekBatch :: (Remote -> Key -> URLString -> Annex ()) -> RegisterUrlOptions -> BatchFormat -> CommandSeek
seekBatch a o fmt = batchOnly Nothing (keyUrlPairs o) $
	batchInput fmt (pure . parsebatch) $
		batchCommandAction . start' "registerurl" a o
  where
	parsebatch l = 
		let (keyname, u) = separate (== ' ') l
		in if null u
			then Left "no url provided"
			else case keyOpt' keyname of
				Left e -> Left e
				Right k -> Right (k, u)

start :: String -> (Remote -> Key -> URLString -> Annex ()) -> RegisterUrlOptions -> [String] -> CommandStart
start msg a o (keyname:url:[]) = start' msg a o (si, (keyOpt keyname, url))
  where
	si = SeekInput [keyname, url]
start _ _ _ _ = giveup "specify a key and an url"

start' :: String -> (Remote -> Key -> URLString -> Annex ()) -> RegisterUrlOptions -> (SeekInput, (Key, URLString)) -> CommandStart
start' msg a o (si, (key, url)) =
	starting msg ai si $
		perform a o key url
  where
	ai = ActionItemOther (Just (UnquotedString url))

perform :: (Remote -> Key -> URLString -> Annex ()) -> RegisterUrlOptions -> Key -> URLString -> CommandPerform
perform a o key url = do
	needremote <- maybe (pure Nothing) (Just <$$> getParsed) (remoteOption o)
	r <- case needremote of
		Just nr | Remote.uuid nr == webUUID -> pure nr
		_ -> Remote.claimingUrl url
	case needremote of
		Just nr | nr /= r -> do
			showNote $ UnquotedString $ "The url " ++ url ++ " is claimed by remote " ++ Remote.name r
			next $ return False
		_ -> do
			a r key (setDownloader' url r)
			next $ return True

registerUrl :: Remote -> Key -> String -> Annex ()
registerUrl remote key url = do
	setUrlPresent key url
	-- setUrlPresent only updates location tracking when the url
	-- does not have an OtherDownloader, but this command needs to do
	-- it for urls claimed by other remotes as well.
	case snd (getDownloader url) of
		OtherDownloader -> logChange NoLiveUpdate key (Remote.uuid remote) InfoPresent
		_ -> return ()

{- git-annex command
 -
 - Copyright 2015-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.CheckPresentKey where

import Command
import qualified Remote
import Remote.List
import Utility.SafeOutput

cmd :: Command
cmd = noCommit $ noMessages $
	command "checkpresentkey" SectionPlumbing
		"check if key is present in remote"
		(paramPair paramKey (paramOptional paramRemote))
		(seek <$$> optParser)

data CheckPresentKeyOptions = CheckPresentKeyOptions
	{ params :: CmdParams
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser CheckPresentKeyOptions
optParser desc = CheckPresentKeyOptions
	<$> cmdParams desc
	<*> parseBatchOption False

seek :: CheckPresentKeyOptions -> CommandSeek
seek o = case batchOption o of
	NoBatch -> case params o of
		(ks:rn:[]) -> toRemote rn >>= (check ks . Just) >>= exitResult
		(ks:[]) -> check ks Nothing >>= exitResult
		_ -> wrongnumparams
	Batch fmt -> do
		checker <- case params o of
			(rn:[]) -> toRemote rn >>= \r -> return (flip check (Just r))
			[] -> return (flip check Nothing)
			_ -> wrongnumparams
		batchInput fmt (pure . Right) $
			checker . snd >=> batchResult
  where
	wrongnumparams = giveup "Wrong number of parameters"
					
data Result = Present | NotPresent | CheckFailure String

check :: String -> Maybe Remote -> Annex Result
check ks mr = case mr of
	Just r -> go Nothing [r]
	Nothing -> do
		mostlikely <- Remote.keyPossibilities k
		otherremotes <- flip Remote.remotesWithoutUUID 
			(map Remote.uuid mostlikely)
			<$> remoteList
		go Nothing (mostlikely ++ otherremotes)
  where
	k = toKey ks
	go Nothing [] = return NotPresent
	go (Just e) [] = return $ CheckFailure e
	go olderr (r:rs) = Remote.hasKey r k >>= \case
		Right True -> return Present
		Right False -> go olderr rs
		Left e -> go (Just e) rs

exitResult :: Result -> Annex a
exitResult Present = liftIO exitSuccess
exitResult NotPresent = liftIO exitFailure
exitResult (CheckFailure msg) = liftIO $ do
	hPutStrLn stderr (safeOutput msg)
	exitWith $ ExitFailure 100

batchResult :: Result -> Annex ()
batchResult Present = liftIO $ putStrLn "1"
batchResult _ = liftIO $ putStrLn "0"

toKey :: String -> Key
toKey = fromMaybe (giveup "Bad key") . deserializeKey

toRemote :: String -> Annex Remote
toRemote rn = maybe (giveup "Unknown remote") return
	=<< Remote.byNameWithUUID (Just rn)

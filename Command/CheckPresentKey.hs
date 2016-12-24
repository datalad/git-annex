{- git-annex command
 -
 - Copyright 2015-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.CheckPresentKey where

import Command
import qualified Remote

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
	<*> parseBatchOption

seek :: CheckPresentKeyOptions -> CommandSeek
seek o = case batchOption o of
	NoBatch -> case params o of
		(ks:rn:[]) -> toRemote rn >>= (check ks . Just) >>= exitResult
		(ks:[]) -> check ks Nothing >>= exitResult
		_ -> wrongnumparams
	Batch -> do
		checker <- case params o of
			(rn:[]) -> toRemote rn >>= \r -> return (flip check (Just r))
			[] -> return (flip check Nothing)
			_ -> wrongnumparams
		batchInput Right $ checker >=> batchResult
  where
	wrongnumparams = giveup "Wrong number of parameters"
					
data Result = Present | NotPresent | CheckFailure String

check :: String -> Maybe Remote -> Annex Result
check ks mr = case mr of
	Nothing -> go Nothing =<< Remote.keyPossibilities k
	Just r -> go Nothing [r]
  where
	k = toKey ks
	go Nothing [] = return NotPresent
	go (Just e) [] = return $ CheckFailure e
	go olderr (r:rs) = do
		v <- Remote.hasKey r k
		case v of
			Right True -> return Present
			Right False -> go olderr rs
			Left e -> go (Just e) rs

exitResult :: Result -> Annex a
exitResult Present = liftIO exitSuccess
exitResult NotPresent = liftIO exitFailure
exitResult (CheckFailure msg) = liftIO $ do
	hPutStrLn stderr msg
	exitWith $ ExitFailure 100

batchResult :: Result -> Annex ()
batchResult Present = liftIO $ putStrLn "1"
batchResult _ = liftIO $ putStrLn "0"

toKey :: String -> Key
toKey = fromMaybe (giveup "Bad key") . file2key

toRemote :: String -> Annex Remote
toRemote rn = maybe (giveup "Unknown remote") return
	=<< Remote.byNameWithUUID (Just rn)

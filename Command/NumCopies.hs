{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.NumCopies where

import Command
import qualified Annex
import Annex.NumCopies
import Types.Messages

cmd :: Command
cmd = command "numcopies" SectionSetup 
	"configure desired number of copies"
	paramNumber (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start [] = startGet
start [s] = case readish s of
	Nothing -> error $ "Bad number: " ++ s
	Just n
		| n > 0 -> startSet n
		| n == 0 -> ifM (Annex.getState Annex.force)
			( startSet n
			, error "Setting numcopies to 0 is very unsafe. You will lose data! If you really want to do that, specify --force."
			)
		| otherwise -> error "Number cannot be negative!"
start _ = error "Specify a single number."

startGet :: CommandStart
startGet = next $ next $ do
	Annex.setOutput QuietOutput
	v <- getGlobalNumCopies
	case v of
		Just n -> liftIO $ putStrLn $ show $ fromNumCopies n
		Nothing -> do
			liftIO $ putStrLn "global numcopies is not set"
			old <- deprecatedNumCopies
			case old of
				Nothing -> liftIO $ putStrLn "(default is 1)"
				Just n -> liftIO $ putStrLn $ "(deprecated git config annex.numcopies is set to " ++ show (fromNumCopies n) ++ " locally)"
	return True

startSet :: Int -> CommandStart
startSet n = do
	showStart "numcopies" (show n)
	next $ next $ do
		setGlobalNumCopies $ NumCopies n
		return True

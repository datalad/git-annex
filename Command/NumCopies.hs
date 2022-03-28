{- git-annex command
 -
 - Copyright 2014-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.NumCopies where

import Command
import Annex.NumCopies

cmd :: Command
cmd = noMessages $ command "numcopies" SectionSetup 
	"configure desired number of copies"
	paramNumber (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start = start' "numcopies" startGet startSet

start' :: String -> CommandStart -> (Int -> CommandStart) -> [String] -> CommandStart
start' _ startget _ [] = startget
start' setting _ startset [s] = case readish s of
	Nothing -> giveup $ "Bad number: " ++ s
	Just n
		| n > 0 -> startset n
		| n == 0 -> giveup $ "Cannot set " ++ setting ++ " to 0."
		| otherwise -> giveup $ setting ++ " cannot be negative!"
start' _ _ _ _ = giveup "Specify a single number."

startGet :: CommandStart
startGet = startingCustomOutput (ActionItemOther Nothing) $ next $ do
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
startSet n = startingUsualMessages "numcopies" ai si $ do
	setGlobalNumCopies $ configuredNumCopies n
	next $ return True
  where
	ai = ActionItemOther (Just $ show n)
	si = SeekInput [show n]

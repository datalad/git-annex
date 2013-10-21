{- git-recover-repository program
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import System.Environment
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import qualified Data.Set as S

import Common
import qualified Git.CurrentRepo
import qualified Git.RecoverRepository
import qualified Git.Config
import qualified Git.Branch

header :: String
header = "Usage: git-recover-repository"

usage :: a
usage = error $ "bad parameters\n\n" ++ header

parseArgs :: IO Bool
parseArgs = do
	args <- getArgs
	return $ or $ map parse args
  where
	parse "--force" = True
	parse _ = usage

enableDebugOutput :: IO ()
enableDebugOutput = do
	s <- setFormatter
		<$> streamHandler stderr DEBUG -- NOTICE
		<*> pure (simpleLogFormatter "$msg")
	updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [s])

main :: IO ()
main = do
	enableDebugOutput
	forced <- parseArgs
	
	g <- Git.Config.read =<< Git.CurrentRepo.get
	missing <- Git.RecoverRepository.cleanCorruptObjects g
	stillmissing <- Git.RecoverRepository.retrieveMissingObjects missing g
	if S.null stillmissing
		then putStr $ unlines
			[ "Successfully recovered repository!"
			, "You should run \"git fsck\" to make sure, but it looks like"
			, "everything was recovered ok."
			]
		else do
			putStrLn $ unwords
				[ show (S.size stillmissing)
				, "missing objects could not be recovered!"
				]
			if forced
				then do
					(remotebranches, goodcommits) <- Git.RecoverRepository.removeTrackingBranches stillmissing Git.RecoverRepository.emptyGoodCommits g
					unless (null remotebranches) $
						putStrLn $ unwords
							[ "removed"
							, show (length remotebranches)
							, "remote tracking branches that referred to missing objects"
							]
					(resetbranches, deletedbranches, _) <- Git.RecoverRepository.resetLocalBranches stillmissing goodcommits g
					unless (null resetbranches) $ do
						putStrLn "Reset these local branches to old versions before the missing objects were committed:"
						putStr $ unlines $ map show resetbranches
					unless (null deletedbranches) $ do
						putStrLn "Deleted these local branches, which could not be recovered due to missing objects:"
						putStr $ unlines $ map show deletedbranches
					mcurr <- Git.Branch.currentUnsafe g
					case mcurr of
						Nothing -> return ()
						Just curr -> when (any (== curr) (resetbranches ++ deletedbranches)) $ do
							putStrLn $ unwords
								[ "You currently have"
								, show curr
								, "checked out. You may have staged changes in the index that can be committed to recover the lost state of this branch!"
								]
				else putStrLn "To force a recovery to a usable state, run this command again with the --force parameter."

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
import qualified Git
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
		<$> streamHandler stderr NOTICE
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
					printList (map show resetbranches)
						"Reset these local branches to old versions before the missing objects were committed:"
					printList (map show deletedbranches)
						"Deleted these local branches, which could not be recovered due to missing objects:"
					deindexedfiles <- Git.RecoverRepository.rewriteIndex stillmissing g
					printList deindexedfiles
						"Removed these missing files from the index. You should look at what files are present in your working tree and git add them back to the index when appropriate."
					unless (Git.repoIsLocalBare g) $ do
						mcurr <- Git.Branch.currentUnsafe g
						case mcurr of
							Nothing -> return ()
							Just curr -> when (any (== curr) (resetbranches ++ deletedbranches)) $ do
								putStrLn $ unwords
									[ "You currently have"
									, show curr
									, "checked out. You may have staged changes in the index that can be committed to recover the lost state of this branch!"
									]
				else if Git.repoIsLocalBare g
					then do
						putStrLn "If you have a clone of this bare repository, you should add it as a remote of this repository, and re-run git-recover-repository."
						putStrLn "If there are no clones of this repository, you can instead run git-recover-repository with the --force parameter to force recovery to a possibly usable state."
					else putStrLn "To force a recovery to a usable state, run this command again with the --force parameter."

printList :: [String] -> String -> IO ()
printList items header
	| null items = return ()
	| otherwise = do
		putStrLn header
		putStr $ unlines $ map (\i -> "\t" ++ i) truncateditems
  where
  	numitems = length items
	truncateditems
		| numitems > 10 = take 10 items ++ ["(and " ++ show (numitems - 10) ++ " more)"]
		| otherwise = items

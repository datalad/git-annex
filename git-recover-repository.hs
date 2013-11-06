{- git-recover-repository program
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import System.Environment
import qualified Data.Set as S
import Data.Tuple.Utils

import Common
import qualified Git
import qualified Git.CurrentRepo
import qualified Git.Fsck
import qualified Git.Repair
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

main :: IO ()
main = do
	forced <- parseArgs
	
	g <- Git.Config.read =<< Git.CurrentRepo.get
	ifM (fst3 <$> Git.Repair.runRepair forced g)
		( exitSuccess
		, exitFailure
		)

{- git-union-merge program
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import System.Environment

import Common
import qualified Git.UnionMerge
import qualified Git.Config
import qualified Git.CurrentRepo
import qualified Git.Branch
import qualified Git.Index
import qualified Git

header :: String
header = "Usage: git-union-merge ref ref newref"

usage :: IO a
usage = error $ "bad parameters\n\n" ++ header

tmpIndex :: Git.Repo -> FilePath
tmpIndex g = Git.localGitDir g </> "index.git-union-merge"

setup :: Git.Repo -> IO ()
setup = cleanup -- idempotency

cleanup :: Git.Repo -> IO ()
cleanup g = nukeFile $ tmpIndex g

parseArgs :: IO [String]
parseArgs = do
	args <- getArgs
	if length args /= 3
		then usage
		else return args

main :: IO ()
main = do
	[aref, bref, newref] <- map Git.Ref <$> parseArgs
	g <- Git.Config.read =<< Git.CurrentRepo.get
	_ <- Git.Index.override (tmpIndex g) g
	setup g
	Git.UnionMerge.merge aref bref g
	_ <- Git.Branch.commit Git.Branch.ManualCommit False "union merge" newref [aref, bref] g
	cleanup g

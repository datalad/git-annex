import Utility.Process
import Data.Maybe
import System.IO
import Control.Concurrent.Async

main = do
	(Nothing, Nothing, Just h, p) <- createProcess $ (proc "./bench" [])
		{ std_err = CreatePipe }
	t <- async $ go h p
	exitcode <- waitForProcess p
	print ("process exited", exitcode)
	wait t
  where
	go h p = do
		l <- hGetLineUntilExitOrEOF p h
		print ("got line", l)
		if isJust l
			then go h p
			else print "at EOF"

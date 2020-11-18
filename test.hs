import Utility.Process
import Data.Maybe
import System.IO
import Control.Concurrent.Async

main = do
	(Nothing, Nothing, Just h, p) <- createProcess $ (proc "./bench" [])
		{ std_err = CreatePipe }
	hSetNewlineMode h universalNewlineMode
	t <- async $ go h p
	exitcode <- waitForProcess p
	print ("process exited", exitcode)
	wait t
  where
	go h p = do
		eof <- hIsEOF h
		if eof
			then return ()
			else do
				l <- hGetLineUntilExitOrEOF p h
				print ("got line", l)
				go h p


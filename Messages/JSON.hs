{- git-annex JSON output
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Messages.JSON (
	start,
	end,
	note,
	add
) where

import Text.JSON

import qualified Utility.JSONStream as Stream

start :: String -> String -> IO ()
start command file = putStr $ Stream.start [("command", command), ("file", file)]

end :: Bool -> IO ()
end b = putStr $ Stream.add [("success", b)] ++ Stream.end

note :: String -> IO ()
note s = add [("note", s)]

add :: JSON a => [(String, a)] -> IO ()
add v = putStr $ Stream.add v

{- git-annex command-line JSON output and input
 -
 - Copyright 2011-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Messages.JSON (
	start,
	end,
	note,
	add,
	complete,
	DualDisp(..),
) where

import Text.JSON

import qualified Utility.JSONStream as Stream
import Types.Key
import Data.Maybe

start :: String -> Maybe FilePath -> Maybe Key -> IO ()
start command file key = putStr $ Stream.start $ Stream.JSONObject $ catMaybes
	[ part "command" (Just command)
	, part "file" file
	, part "key" (fmap key2file key)
	]
  where
	part _ Nothing = Nothing
	part l (Just v) = Just (l, v)

end :: Bool -> IO ()
end b = putStr $ Stream.add (Stream.JSONObject [("success", b)]) ++ Stream.end

note :: String -> IO ()
note s = add (Stream.JSONObject [("note", s)])

add :: Stream.JSONChunk a -> IO ()
add = putStr . Stream.add

complete :: Stream.JSONChunk a -> IO ()
complete v = putStr $ Stream.start v ++ Stream.end

-- A value that can be displayed either normally, or as JSON.
data DualDisp = DualDisp
	{ dispNormal :: String
	, dispJson :: String
	}

instance JSON DualDisp where
	showJSON = JSString . toJSString . dispJson
	readJSON _ = Error "stub"

instance Show DualDisp where
	show = dispNormal

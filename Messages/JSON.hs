{- git-annex command-line JSON output and input
 -
 - Copyright 2011-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Messages.JSON (
	start,
	end,
	note,
	add,
	complete,
	DualDisp(..),
	ObjectMap(..),
	ParsedJSON(..),
) where

import Data.Aeson
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Text as T

import qualified Utility.JSONStream as Stream
import Types.Key
import Data.Maybe

start :: String -> Maybe FilePath -> Maybe Key -> IO ()
start command file key = putStr $ Stream.start $ Stream.JSONChunk $ catMaybes
	[ part "command" (Just command)
	, part "file" file
	, part "key" (fmap key2file key)
	]
  where
	part _ Nothing = Nothing
	part l (Just v) = Just (l, v)

end :: Bool -> IO ()
end b = putStr $ Stream.add (Stream.JSONChunk [("success", b)]) ++ Stream.end

note :: String -> IO ()
note s = add (Stream.JSONChunk [("note", s)])

add :: Stream.JSONChunk a -> IO ()
add = putStr . Stream.add

complete :: Stream.JSONChunk a -> IO ()
complete v = putStr $ Stream.start v ++ Stream.end

-- A value that can be displayed either normally, or as JSON.
data DualDisp = DualDisp
	{ dispNormal :: String
	, dispJson :: String
	}

instance ToJSON DualDisp where
	toJSON = toJSON . dispJson

instance Show DualDisp where
	show = dispNormal

-- A Map that is serialized to JSON as an object, with each key being a
-- field of the object. This is different from Aeson's normal 
-- serialization of Map, which uses "[key, value]".
data ObjectMap a = ObjectMap { fromObjectMap :: M.Map String a }

instance ToJSON a => ToJSON (ObjectMap a) where
	toJSON (ObjectMap m) = object $ map go $ M.toList m
	  where
		go (k, v) = (T.pack k, toJSON v)

-- An Aeson parser for the JSON output by this module, and 
-- similar JSON input from users.
data ParsedJSON a = ParsedJSON
	{ parsedCommand :: Maybe String -- optional
	, parsedKeyfile :: Either FilePath Key -- key is preferred
	, parsedNote :: Maybe String -- optional
	, parsedSuccess :: Bool -- optional, defaults to True
	, parsedAdded :: Maybe a -- to parse additional fields added by `add`
	}
	deriving (Show)

instance FromJSON a => FromJSON (ParsedJSON a) where
	parseJSON (Object v) = ParsedJSON
		<$> (v .:? "command")
		<*> parsekeyfile
		<*> (v .:? "note")
		<*> (v .:? "success" .!= True)
		<*> parseadded
	  where
		parsekeyfile = do
			mks <- v .:? "key"
			case file2key =<< mks of
				Just k -> return (Right k)
				Nothing -> do
					mf <- v .:? "file"
					case mf of
						Just f -> return (Left f)
						Nothing -> fail "missing key or file"
		parseadded = (Just <$> parseJSON (Object v)) <|> return Nothing
	parseJSON _ = mempty

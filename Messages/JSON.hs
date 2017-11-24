{- git-annex command-line JSON output and input
 -
 - Copyright 2011-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, GADTs #-}

module Messages.JSON (
	JSONBuilder,
	JSONChunk(..),
	emit,
	none,
	start,
	end,
	note,
	add,
	complete,
	progress,
	DualDisp(..),
	ObjectMap(..),
	JSONActionItem(..),
) where

import Data.Aeson
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent
import Data.Maybe
import Data.Monoid
import Prelude

import Key
import Utility.Metered
import Utility.Percentage

-- A global lock to avoid concurrent threads emitting json at the same time.
{-# NOINLINE emitLock #-}
emitLock :: MVar ()
emitLock = unsafePerformIO $ newMVar ()

emit :: Object -> IO ()
emit o = do
	takeMVar emitLock
	B.hPut stdout (encode o)
	putStr "\n"
	putMVar emitLock ()

-- Building up a JSON object can be done by first using start,
-- then add and note any number of times, and finally complete.
type JSONBuilder = Maybe (Object, Bool) -> Maybe (Object, Bool)

none :: JSONBuilder
none = id

start :: String -> Maybe FilePath -> Maybe Key -> JSONBuilder
start command file key _ = Just (o, False)
  where
	Object o = toJSON $ JSONActionItem
		{ itemCommand = Just command
		, itemKey = key
		, itemFile = file
		, itemAdded = Nothing
		}

end :: Bool -> JSONBuilder
end b (Just (o, _)) = Just (HM.insert "success" (toJSON b) o, True)
end _ Nothing = Nothing

note :: String -> JSONBuilder
note s (Just (o, e)) = Just (HM.insert "note" (toJSON s) o, e)
note _ Nothing = Nothing

data JSONChunk v where
	AesonObject :: Object -> JSONChunk Object
	JSONChunk :: ToJSON v => [(String, v)] -> JSONChunk [(String, v)]

add :: JSONChunk v -> JSONBuilder
add v (Just (o, e)) = Just (HM.union o' o, e)
  where
	Object o' = case v of
		AesonObject ao -> Object ao
		JSONChunk l -> object (map mkPair l)
	mkPair (s, d) = (T.pack s, toJSON d)
add _ Nothing = Nothing

complete :: JSONChunk v -> JSONBuilder
complete v _ = add v (Just (HM.empty, True))

-- Show JSON formatted progress, including the current state of the JSON 
-- object for the action being performed.
progress :: Maybe Object -> Maybe Integer -> BytesProcessed -> IO ()
progress maction msize bytesprocessed = emit $ case maction of
	Just action -> HM.insert "action" (Object action) o
	Nothing -> o
  where
	n = fromBytesProcessed bytesprocessed :: Integer
	Object o = case msize of
		Just size -> object
			[ "byte-progress" .= n
			, "percent-progress" .= showPercentage 2 (percentage size n)
			, "total-size" .= size
			]
		Nothing -> object
			[ "byte-progress" .= n ]

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

-- An item that a git-annex command acts on, and displays a JSON object about.
data JSONActionItem a = JSONActionItem
	{ itemCommand :: Maybe String
	, itemKey :: Maybe Key
	, itemFile :: Maybe FilePath
	, itemAdded :: Maybe a -- for additional fields added by `add`
	}
	deriving (Show)

instance ToJSON (JSONActionItem a) where
	toJSON i = object $ catMaybes
		[ Just $ "command" .= itemCommand i
		, case itemKey i of
			Nothing -> Nothing
			Just k -> Just $ "key" .= toJSON k
		, Just $ "file" .= itemFile i
		-- itemAdded is not included; must be added later by 'add'
		]

instance FromJSON a => FromJSON (JSONActionItem a) where
	parseJSON (Object v) = JSONActionItem
		<$> (v .:? "command")
		<*> (maybe (return Nothing) parseJSON =<< (v .:? "key"))
		<*> (v .:? "file")
		<*> parseadded
	  where
		parseadded = (Just <$> parseJSON (Object v)) <|> return Nothing
	parseJSON _ = mempty

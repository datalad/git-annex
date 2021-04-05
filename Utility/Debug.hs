{- Debug output
 -
 - Copyright 2021 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs -w #-}

module Utility.Debug (
	DebugSource(..),
	DebugSelector(..),
	configureDebug,
	getDebugSelector,
	debug,
	fastDebug
) where

import qualified Data.ByteString as S
import Data.IORef
import Data.String
import Data.Time
import System.IO.Unsafe (unsafePerformIO)

import Utility.FileSystemEncoding

-- | The source of a debug message. For example, this could be a module or
-- function name.
newtype DebugSource = DebugSource S.ByteString
	deriving (Eq, Show)

instance IsString DebugSource where
	fromString = DebugSource . encodeBS'

-- | Selects whether to display a message from a source.
newtype DebugSelector = DebugSelector (DebugSource -> Bool)

-- | Configures debugging.
configureDebug
	:: (S.ByteString -> IO ())
	-- ^ Used to display debug output.
	-> DebugSelector
	-> IO ()
configureDebug src p = writeIORef debugConfigGlobal (src, p)

-- | Gets the currently configured DebugSelector.
getDebugSelector :: IO DebugSelector
getDebugSelector = snd <$> readIORef debugConfigGlobal

-- A global variable for the debug configuration.
{-# NOINLINE debugConfigGlobal #-}
debugConfigGlobal :: IORef (S.ByteString -> IO (), DebugSelector)
debugConfigGlobal = unsafePerformIO $ newIORef (dontshow, selectnone)
  where
	dontshow _ = return ()
	selectnone = DebugSelector (\_ -> False)

-- | Displays a debug message, if that has been enabled by configureDebug.
--
-- This is reasonably fast when debugging is not enabled, but since it does
-- have to consult a IORef each time, using it in a tight loop may slow
-- down the program.
debug :: DebugSource -> String -> IO ()
debug src msg = do
	(displayer, DebugSelector p) <- readIORef debugConfigGlobal
	if p src
		then displayer =<< formatDebugMessage src msg
		else return ()

-- | Displays a debug message, if the DebugSelector allows.
--
-- When the DebugSelector does not let the message be displayed, this runs
-- very quickly, allowing it to be used inside tight loops.
fastDebug :: DebugSelector -> DebugSource -> String -> IO ()
fastDebug (DebugSelector p) src msg
	| p src = do
		(displayer, _) <- readIORef debugConfigGlobal
		displayer =<< formatDebugMessage src msg
	| otherwise = return ()

formatDebugMessage :: DebugSource -> String -> IO S.ByteString
formatDebugMessage (DebugSource src) msg = do
	t <- encodeBS' . formatTime defaultTimeLocale "[%F %X%Q]"
		<$> getZonedTime
	return (t <> " (" <> src <> ") " <> encodeBS msg)

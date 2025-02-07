{- git long-running filter process
 -
 - As documented in git's gitattributes(5) and
 - Documentation/technical/long-running-process-protocol.txt
 -
 - Copyright 2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.FilterProcess (
	WelcomeMessage(..),
	Version(..),
	Capability(..),
	longRunningProcessHandshake,
	longRunningFilterProcessHandshake,
	FilterRequest(..),
	getFilterRequest,
	respondFilterRequest,
) where

import Common
import Git.PktLine

import qualified Data.ByteString as B

{- This is a message like "git-filter-client" or "git-filter-server" -}
data WelcomeMessage = WelcomeMessage PktLine
	deriving (Show)

{- Configuration message, eg "foo=bar" -}
data ConfigValue = ConfigValue String String
	deriving (Show, Eq)

encodeConfigValue :: ConfigValue -> PktLine
encodeConfigValue (ConfigValue k v) = stringPktLine (k <> "=" <> v)

decodeConfigValue :: PktLine -> Maybe ConfigValue
decodeConfigValue pktline =
	let t = pktLineToString pktline
	    (k, v) = break (== '=') t
	in if null v
		then Nothing
		else Just $ ConfigValue k (drop 1 v)

extractConfigValue :: [ConfigValue] -> String -> Maybe String
extractConfigValue [] _ = Nothing
extractConfigValue (ConfigValue k v:cs) wantk
	| k == wantk = Just v
	| otherwise = extractConfigValue cs wantk

data Version = Version Int
	deriving (Show, Eq)

encodeVersion :: Version -> PktLine
encodeVersion (Version n) = encodeConfigValue $ ConfigValue "version" (show n)

decodeVersion :: PktLine -> Maybe Version
decodeVersion pktline = decodeConfigValue pktline >>= \case
	ConfigValue "version" v -> Version <$> readish v
	_ -> Nothing

data Capability = Capability String
	deriving (Show, Eq)

encodeCapability :: Capability -> PktLine
encodeCapability (Capability c) = encodeConfigValue $ 
	ConfigValue "capability" c

decodeCapability :: PktLine -> Maybe Capability
decodeCapability pktline = decodeConfigValue pktline >>= \case
	ConfigValue "capability" c -> Just $ Capability c
	_ -> Nothing

longRunningProcessHandshake
	:: (WelcomeMessage -> Maybe WelcomeMessage)
	-> ([Version] -> [Version])
	-> ([Capability] -> [Capability])
	-> IO (Either String ())
longRunningProcessHandshake respwelcomemessage filterversions filtercapabilities =
	readUntilFlushPkt >>= \case
		[] -> protoerr "no welcome message"
		(welcomemessage:versions) -> 
			checkwelcomemessage welcomemessage $
				checkversion versions $ do
					capabilities <- readUntilFlushPkt
					checkcapabilities capabilities success
  where
	protoerr msg = return $ Left $ "git protocol error: " ++ msg
	success = return (Right ())

	checkwelcomemessage welcomemessage cont =
		case respwelcomemessage (WelcomeMessage welcomemessage) of
			Nothing -> protoerr "unsupported welcome message"
			Just (WelcomeMessage welcomemessage') -> do
				writePktLine stdout welcomemessage'
				cont

	checkversion versions cont = do
		let versions' = filterversions (mapMaybe decodeVersion versions)
		if null versions'
			then protoerr "unsupported protocol version"
			else do
				forM_ versions' $ \v ->
					writePktLine stdout $ encodeVersion v
				writePktLine stdout flushPkt
				cont
	
	checkcapabilities capabilities cont = do
		let capabilities' = filtercapabilities (mapMaybe decodeCapability capabilities)
		if null capabilities'
			then protoerr "unsupported protocol capabilities"
			else do
				forM_ capabilities' $ \c ->
					writePktLine stdout $ encodeCapability c
				writePktLine stdout flushPkt
				cont

longRunningFilterProcessHandshake :: IO (Either String ())
longRunningFilterProcessHandshake =
	longRunningProcessHandshake respwelcomemessage filterversions filtercapabilities
  where
	respwelcomemessage (WelcomeMessage w)
		| pktLineToString w == "git-filter-client" =
			Just $ WelcomeMessage $ stringPktLine "git-filter-server"
		| otherwise = Nothing
	filterversions = filter (== Version 2)
	-- Delay capability is not implemented, so filter it out.
	filtercapabilities = filter (`elem` [Capability "smudge", Capability "clean"])

data FilterRequest = Smudge OsPath | Clean OsPath
	deriving (Show, Eq)

{- Waits for the next FilterRequest to be received. Does not read
 - the content to be filtered, which is what gets sent subsequent to the
 - FilterRequest. Use eg readUntilFlushPkt to read it, before calling
 - respondFilterRequest. -}
getFilterRequest :: IO (Maybe FilterRequest)
getFilterRequest = do
	ps <- readUntilFlushPkt
	let cs = mapMaybe decodeConfigValue ps
	case (extractConfigValue cs "command", extractConfigValue cs "pathname") of
		(Just command, Just pathname)
			| command == "smudge" -> return $ Just $ Smudge $ toOsPath pathname
			| command == "clean" -> return $ Just $ Clean $ toOsPath pathname
			| otherwise -> return Nothing
		_ -> return Nothing

{- Send a response to a FilterRequest, consisting of the filtered content. -}
respondFilterRequest :: B.ByteString -> IO ()
respondFilterRequest b = do
	writePktLine stdout $ encodeConfigValue $ ConfigValue "status" "success"
	writePktLine stdout flushPkt
	send b
	-- The protocol allows for another list of ConfigValues to be sent
	-- here, but we don't use it. Send another flushPkt to terminate
	-- the empty list.
	writePktLine stdout flushPkt
  where
	send b' = 
		let (pktline, rest) = encodePktLine b'
		in do
			if isFlushPkt pktline
				then return ()
				else writePktLine stdout pktline
			case rest of
				Just b'' -> send b''
				Nothing -> writePktLine stdout flushPkt

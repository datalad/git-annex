{- Git pkt-line format, as documented in 
 - git/Documentation/technical/protocol-common.txt
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.Protocol.PktLine (
	PktLine,
	flushPkt,
	textPktLine,
	pktLineText,
	streamPktLine,
	encodePktLine,
	parsePktLine,
	decodePktLine,
	splitPktLine,
) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.ByteString.Lazy as AL
import Data.ByteString.Builder
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as E
import Data.Monoid
import Data.Word

-- | A pkt-line encodes a variable length binary string with a maximum size.
--
-- This module only exports smart constructors for legal pkt-lines.
newtype PktLine = PktLine S.ByteString
	deriving (Show)

-- | Maximum data that can be contained in a pkt-line, not
-- including the 4 byte length header.
maxPktLineContent :: Word16
maxPktLineContent = 65516

-- | Maximum length of a pkt-line, including the 4 byte size header.
maxPktLineLength :: Word16
maxPktLineLength = 65520

-- | The flush-pkt, a special case in the protocol that is often used to
-- eg, signal the end of a stream of binary data.
flushPkt :: PktLine
flushPkt = PktLine S.empty

-- | Encodes a Text as a PktLine. Fails if the Text it too large.
--
-- A trailing newline is included after it, as the protocol recommends
-- doing for non-binary data.
textPktLine :: T.Text -> Maybe PktLine
textPktLine t = 
	let b = E.encodeUtf8 t <> "\n"
	in if S.length b > fromIntegral maxPktLineContent
		then Nothing
		else Just (PktLine b)

-- | Extracts Text from a PktLine. Any trailing newline is removed.
pktLineText :: PktLine -> Either E.UnicodeException T.Text
pktLineText (PktLine b) = case E.decodeUtf8' b of
	Left e -> Left e
	Right t -> 
		let (t', end) = T.splitAt (T.length t - 1) t
		in if end == "\n"
			then Right t'
			else Right t

-- | Creates a stream of PktLines encoding a lazy ByteString of any size.
-- Note that the stream is not terminated with a flushPkt.
streamPktLine :: L.ByteString -> [PktLine]
streamPktLine l =
	let (chunk, rest) = L.splitAt (fromIntegral maxPktLineContent) l
	    pktline = PktLine $ mconcat $ L.toChunks chunk
	in if L.null rest
		then pktline : []
		else pktline : streamPktLine rest

-- | ByteString builder for a pkt-line.
encodePktLine :: PktLine -> Builder
encodePktLine (PktLine b)
	-- Avoid sending an empty pkt-line; send a flush-pkt instead.
	| S.null b = "0000"
	| otherwise = lengthheader <> byteString b
  where
	-- The length header is always 4 bytes long, and includes
	-- itself in its length.
	lengthheader = word16HexFixed (fromIntegral (S.length b) + 4)

-- | Attoparsec parser for a pkt-line.
parsePktLine :: Parser PktLine
parsePktLine = do
	len <- parseLengthHeader
	if len == 0
		then do
			endOfInput
			return flushPkt
		else if len < 4
			-- It's impossible for a pkt-line to be less than
			-- 4 bytes long, since the length header is 4 bytes.
			then fail "invalid pkt-line length"
			else PktLine <$> A.take (len - 4)

parseLengthHeader :: Parser Int
parseLengthHeader = do
	-- Attoparsec's hexidecimal parser will consume any amount
	-- of hex, but the length header is limited to 4 bytes, so
	-- take those and parse only them.
	h <- A.take 4
	-- Require all 4 bytes to be hexidecimal by using endOfInput.
	case parseOnly (hexadecimal <* endOfInput) h of
		Left e -> fail e
		Right len
			| len > fromIntegral maxPktLineLength ->
				fail "pkt-line too long"
			| otherwise -> return len

-- | The ByteString must contain only a pkt-line with no additional data
-- or this will fail.
decodePktLine :: S.ByteString -> Either String PktLine
decodePktLine = parseOnly (parsePktLine <* endOfInput)

-- | Split the next PktLine from a lazy ByteString, returning it and the
-- remainder of the ByteString.
splitPktLine :: L.ByteString -> Either String (PktLine, L.ByteString)
splitPktLine = go . AL.parse parsePktLine
  where
	go (AL.Done rest p) = Right (p, rest)
	go (AL.Fail _ _ e) = Left e

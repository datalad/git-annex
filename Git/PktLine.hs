{- git pkt-line communication format
 -
 - As documented in git's Documentation/technical/protocol-common.txt
 -
 - Copyright 2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.PktLine (
	PktLine,
	pktLineToByteString,
	pktLineToString,
	readPktLine,
	encodePktLine,
	stringPktLine,
	writePktLine,
	flushPkt,
	isFlushPkt,
	readUntilFlushPkt,
	readUntilFlushPktOrSize,
	discardUntilFlushPkt,
) where

import System.IO
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Text.Printf

import Utility.PartialPrelude
import Utility.FileSystemEncoding

{- This is a variable length binary string, but its size is limited to
 - maxPktLineLength. Its serialization includes a 4 byte hexadecimal
 - prefix giving its total length, including that prefix. -}
newtype PktLine = PktLine B.ByteString
	deriving (Show)

{- Maximum allowed length of the string encoded in PktLine
 - is slightly shorter than the absolute maximum possible length.
 - Git does not accept anything longer than this. -}
maxPktLineLength :: Int
maxPktLineLength = 65520 - pktLineHeaderLength

pktLineHeaderLength :: Int
pktLineHeaderLength = 4

pktLineToByteString :: PktLine -> B.ByteString
pktLineToByteString (PktLine b) = b

{- When the pkt-line contains non-binary data, its serialization
 - may include a terminating newline. This removes that newline, if it was
 - present.
 -
 - Note that the pkt-line has no defined encoding, and could still
 - contain something non-ascii, eg a filename. -}
pktLineToString :: PktLine -> String
pktLineToString (PktLine b) = 
	let s = decodeBS b
	in case lastMaybe s of
		Just '\n' -> beginning s
		_ -> s

{- Reads the next PktLine from a Handle. Returns Nothing on EOF or when
 - there is a protocol error. -}
readPktLine :: Handle -> IO (Maybe PktLine)
readPktLine h = do
	lenb <- B.hGet h pktLineHeaderLength
	if B.length lenb < pktLineHeaderLength
		then return Nothing
		else case A8.parseOnly (A8.hexadecimal <* A8.endOfInput) lenb of
			Right len -> go (len - pktLineHeaderLength) mempty
			_ -> return Nothing
  where
	go n b
		| n <= 0 = return (Just (PktLine b))
		| otherwise = do
			b' <- B.hGet h n
			if B.length b' == 0
				then return Nothing -- EOF
				else go (n - B.length b') (b <> b')

{- Encodes the ByteString as a PktLine. But if the ByteString is too
 - long to fit in a single PktLine, returns the remainder of it. -}
encodePktLine :: B.ByteString -> (PktLine, Maybe B.ByteString)
encodePktLine b
	| B.length b > maxPktLineLength =
		let (b', rest) = B.splitAt maxPktLineLength b
		in (PktLine b', Just rest)
	| otherwise = (PktLine b, Nothing)

{- If the String is too long to fit in a single PktLine,
 - will throw an error. -}
stringPktLine :: String -> PktLine
stringPktLine s
	| length s > maxPktLineLength =
		error "textPktLine called with too-long value"
	| otherwise = PktLine (encodeBS s <> "\n")

{- Sends a PktLine to a Handle, and flushes it so that it will be
 - visible to the Handle's reader. -}
writePktLine :: Handle -> PktLine -> IO ()
writePktLine h (PktLine b)
	-- Special case for empty string; avoid encoding as "0004".
	| B.null b = do
		B.hPut h "0000"
		hFlush h
	| otherwise = do
		hPutStr h $ printf "%04x" (B.length b + pktLineHeaderLength)
		B.hPut h b
		hFlush h

flushPkt :: PktLine
flushPkt = PktLine mempty

isFlushPkt :: PktLine -> Bool
isFlushPkt (PktLine b) = b == mempty

{- Reads PktLines until a flushPkt (or EOF), 
 - and returns all except the flushPkt -}
readUntilFlushPkt :: IO [PktLine]
readUntilFlushPkt = go []
  where
	go l = readPktLine stdin >>= \case
		Just pktline | not (isFlushPkt pktline) -> go (pktline:l)
		_ -> return (reverse l)

{- Reads PktLines until at least the specified number of bytes have been
 - read, or until a flushPkt (or EOF). Returns Right if it did read a
 - flushPkt/EOF, and Left if there is still content leftover that needs to
 - be read. -}
readUntilFlushPktOrSize :: Int -> IO (Either [PktLine] [PktLine])
readUntilFlushPktOrSize = go []
  where
	go l n = readPktLine stdin >>= \case
		Just pktline
			| isFlushPkt pktline -> return (Right (reverse l))
			| otherwise -> 
				let len = B.length (pktLineToByteString pktline)
				    n' = n - len
				in if n' <= 0
					then return (Left (reverse (pktline:l)))
					else go (pktline:l) n'
		Nothing -> return (Right (reverse l))

{- Reads PktLines until a flushPkt (or EOF), and throws them away. -}
discardUntilFlushPkt :: IO ()
discardUntilFlushPkt = readPktLine stdin >>= \case
	Just pktline | isFlushPkt pktline -> return ()
	Nothing -> return ()
	_ -> discardUntilFlushPkt

{- line based log files
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Line where

import Common

import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 (isEndOfLine)
import qualified Data.DList as D

{- Applies a parser to each line of a log file.
 -
 - If the parser fails to parse a line, that line is skipped, instead of
 - the overall parse failing. This is generally a good idea in case a newer
 - version of git-annex somehow changed the format of the log file.
 -
 - Any combination of \r and \n are taken to be the end of the line.
 - (Some versions of git-annex on Windows wrote \r into git-annex branch
 - files, and multiple \r's sometimes accumulated.)
 -
 - The parser does not itself need to avoid parsing beyond the end of line;
 - this is implemented only pass the content of a line to the parser.
 -}
parseLogLines :: A.Parser a -> A.Parser [a]
parseLogLines parser = go D.empty
  where
	go dl = do
		line <- A.takeTill isEndOfLine
		A.skipWhile isEndOfLine
		let dl' = case A.parseOnly parser line of
			Left _ -> dl
			Right v -> D.snoc dl v
		(A.endOfInput *> return (D.toList dl')) <|> go dl'

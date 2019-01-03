{- 

The Glasgow Haskell Compiler License

Copyright 2001, The University Court of the University of Glasgow.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.

- Neither name of the University nor the names of its contributors may be
used to endorse or promote products derived from this software without
specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.

-}

module Logs.Line where

import Common

import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.Attoparsec.ByteString.Char8 (isEndOfLine)
import qualified Data.DList as D

-- This is the same as Data.List.lines, with \r added.
-- This works around some versions of git-annex which wrote \r
-- into git-annex branch files on Windows. Those \r's sometimes
-- accumulated over time, so a single line could end with multiple \r's
-- before the \n.
splitLines :: String -> [String]
splitLines "" =  []
splitLines s =  cons (case break (\c -> c == '\n' || c == '\r') s of
	(l, s') -> (l, case s' of
		[]      -> []
		_:s''   -> splitLines s''))
  where
	cons ~(h, t) = h : t

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

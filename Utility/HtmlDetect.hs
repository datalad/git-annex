{- html detection
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.HtmlDetect (
	isHtml,
	isHtmlBs,
	htmlPrefixLength,
) where

import Text.HTML.TagSoup
import Data.Char
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8

-- | Detect if a String is a html document.
--
-- The document many not be valid, or may be truncated, and will
-- still be detected as html, as long as it starts with a
-- "<html>" or "<!DOCTYPE html>" tag.
--
-- Html fragments like "<p>this</p>" are not detected as being html,
-- although some browsers may chose to render them as html.
isHtml :: String -> Bool
isHtml = evaluate . canonicalizeTags . parseTags . take htmlPrefixLength
  where
	evaluate (TagOpen "!DOCTYPE" ((t, _):_):_) = map toLower t == "html"
	evaluate (TagOpen "html" _:_) = True
	-- Allow some leading whitespace before the tag.
	evaluate (TagText t:rest)
		| all isSpace t = evaluate rest
		| otherwise = False
	-- It would be pretty weird to have a html comment before the html
	-- tag, but easy to allow for.
	evaluate (TagComment _:rest) = evaluate rest
	evaluate _ = False

-- | Detect if a ByteString is a html document.
isHtmlBs :: B.ByteString -> Bool
-- The encoding of the ByteString is not known, but isHtml only
-- looks for ascii strings.
isHtmlBs = isHtml . B8.unpack

-- | How much of the beginning of a html document is needed to detect it.
-- (conservatively)
htmlPrefixLength :: Int
htmlPrefixLength = 8192

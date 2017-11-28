{- html detection
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.HtmlDetect where

import Text.HTML.TagSoup
import Data.Char

-- | Detect if a string is a html document.
--
-- The document many not be valid, and will still be detected as html,
-- as long as it starts with a "<html>" or "<!DOCTYPE html>" tag.
--
-- Html fragments like "<p>this</p>" are not detected as being html,
-- although some browsers may chose to render them as html.
isHtml :: String -> Bool
isHtml = evaluate . canonicalizeTags . parseTags . shorten
  where
	-- We only care about the beginning of the file,
	-- so although tagsoup parses lazily anyway, truncate it.
	shorten = take 16384
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

{- optparse-applicative additions
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.OptParse where

import Options.Applicative
import Data.Monoid
import Prelude

-- | A switch that can be enabled using --foo and disabled using --no-foo.
--
-- The option modifier is applied to only the option that is *not* enabled
-- by default. For example:
--
-- > invertableSwitch "recursive" True (help "do not recurse into directories")
-- 
-- This example makes --recursive enabled by default, so 
-- the help is shown only for --no-recursive.
invertableSwitch 
	:: String -- ^ long option
	-> Bool -- ^ is switch enabled by default?
	-> Mod FlagFields Bool -- ^ option modifier
	-> Parser Bool
invertableSwitch longopt defv optmod = invertableSwitch' longopt defv
	(if defv then mempty else optmod)
	(if defv then optmod else mempty)

-- | Allows providing option modifiers for both --foo and --no-foo.
invertableSwitch'
	:: String -- ^ long option (eg "foo")
	-> Bool -- ^ is switch enabled by default?
	-> Mod FlagFields Bool -- ^ option modifier for --foo
	-> Mod FlagFields Bool -- ^ option modifier for --no-foo
	-> Parser Bool
invertableSwitch' longopt defv enmod dismod = collapse <$> many
	( flag' True (enmod <> long longopt)
	<|> flag' False (dismod <> long nolongopt)
	)
  where
	nolongopt = "no-" ++ longopt
	collapse [] = defv
	collapse l = last l

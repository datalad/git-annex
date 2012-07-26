{- Yesod stuff
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Yesod where

import System.FilePath

{- Filename of a template, in the templates/ directory. -}
template :: FilePath -> FilePath
template f = "templates" </> f

{- A hamlet template file. -}
hamletTemplate :: FilePath -> FilePath
hamletTemplate f = template f ++ ".hamlet"

{- A julius template file. -}
juliusTemplate :: FilePath -> FilePath
juliusTemplate f = template f ++ ".julius"

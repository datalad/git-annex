{- git ref stuff
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Ref where

import qualified Data.ByteString.Lazy.Char8 as L

import Common
import Git

{- Converts a fully qualified git ref into a user-visible version. -}
describe :: Ref -> String
describe = remove "refs/heads/" . remove "refs/remotes/" . show
	where
		remove prefix s
			| prefix `isPrefixOf` s = drop (length prefix) s
			| otherwise = s

{- Checks if a ref exists. -}
exists :: Ref -> Repo -> IO Bool
exists ref = runBool "show-ref" 
	[Param "--verify", Param "-q", Param $ show ref]

{- Get the sha of a fully qualified git ref, if it exists. -}
sha :: Branch -> Repo -> IO (Maybe Sha)
sha branch repo = process . L.unpack <$> showref repo
	where
		showref = pipeRead [Param "show-ref",
			Param "--hash", -- get the hash
			Param $ show branch]
		process [] = Nothing
		process s = Just $ Ref $ firstLine s

{- List of (refs, branches) matching a given ref spec.
 - Duplicate refs are filtered out. -}
matching :: Ref -> Repo -> IO [(Ref, Branch)]
matching ref repo = do
	r <- pipeRead [Param "show-ref", Param $ show ref] repo
	return $ nubBy uref $ map (gen . words . L.unpack) (L.lines r)
	where
		gen l = (Ref $ head l, Ref $ last l)
		uref (a, _) (b, _) = a == b


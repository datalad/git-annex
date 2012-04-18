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
import Git.Command

import Data.Char (chr)

{- Converts a fully qualified git ref into a user-visible string. -}
describe :: Ref -> String
describe = show . base

{- Often git refs are fully qualified (eg: refs/heads/master).
 - Converts such a fully qualified ref into a base ref (eg: master). -}
base :: Ref -> Ref
base = Ref . remove "refs/heads/" . remove "refs/remotes/" . show
	where
		remove prefix s
			| prefix `isPrefixOf` s = drop (length prefix) s
			| otherwise = s


{- Given a directory such as "refs/remotes/origin", and a ref such as
 - refs/heads/master, yields a version of that ref under the directory,
 - such as refs/remotes/origin/master. -}
under :: String -> Ref -> Ref
under dir r = Ref $ dir </> show (base r)

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

{- List of (refs, branches) matching a given ref spec. -}
matching :: Ref -> Repo -> IO [(Ref, Branch)]
matching ref repo = do
	r <- pipeRead [Param "show-ref", Param $ show ref] repo
	return $ map (gen . L.unpack) (L.lines r)
	where
		gen l = let (r, b) = separate (== ' ') l in
			(Ref r, Ref b)

{- List of (refs, branches) matching a given ref spec.
 - Duplicate refs are filtered out. -}
matchingUniq :: Ref -> Repo -> IO [(Ref, Branch)]
matchingUniq ref repo = nubBy uniqref <$> matching ref repo
	where
		uniqref (a, _) (b, _) = a == b

{- Checks if a String is a legal git ref name.
 -
 - The rules for this are complex; see git-check-ref-format(1) -}
legal :: Bool -> String -> Bool
legal allowonelevel s = all (== False) illegal
	where
		illegal =
			[ any ("." `isPrefixOf`) pathbits
			, any (".lock" `isSuffixOf`) pathbits
			, not allowonelevel && length pathbits < 2
			, contains ".."
			, any (\c -> contains [c]) illegalchars
			, begins "/"
			, ends "/"
			, contains "//"
			, ends "."
			, contains "@{"
			, null s
			]
		contains v = v `isInfixOf` s
		ends v = v `isSuffixOf` s
		begins v = v `isPrefixOf` s

		pathbits = split "/" s
		illegalchars = " ~^:?*[\\" ++ controlchars
		controlchars = chr 0o177 : [chr 0 .. chr (0o40-1)]

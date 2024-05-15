{- git bundles
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.Bundle where

import Common
import Git
import Git.Command

import Data.Char (ord)
import qualified Data.ByteString.Char8 as S8

listHeads :: FilePath -> Repo -> IO [(Sha, Ref)]
listHeads bundle repo = map gen . S8.lines <$>
	pipeReadStrict [Param "bundle", Param "list-heads", File bundle] repo
  where
	gen l = let (s, r) = separate' (== fromIntegral (ord ' ')) l
		in (Ref s, Ref r)

unbundle :: FilePath -> Repo -> IO ()
unbundle bundle = runQuiet [Param "bundle", Param "unbundle", File bundle]

-- Specifies what to include in the bundle.
data BundleSpec = BundleSpec
	{ preRequisiteRef :: Maybe Ref
	-- ^ Do not include this Ref, or any objects reachable from it
	-- in the bundle. This should be an ancestor of the includeRef.
	, includeRef :: Ref
	-- ^ Include this Ref and objects reachable from it in the bundle,
	-- unless filtered out by the preRequisiteRef of this BundleSpec
	-- or any other one that is included in the bundle.
	}
	deriving (Show)

-- Include the ref and all objects reachable from it in the bundle.
-- (Unless another BundleSpec is included that has a preRequisiteRef
-- that filters out the ref or other objects.)
fullBundleSpec :: Ref -> BundleSpec
fullBundleSpec r = BundleSpec
	{ preRequisiteRef = Nothing
	, includeRef = r
	}

create :: FilePath -> [BundleSpec] -> Repo -> IO ()
create bundle revs repo = pipeWrite
	[ Param "bundle"
	, Param "create"
	, Param "--quiet"
	, File bundle
	, Param "--stdin"
	] repo writer
  where
	writer h = do
		forM_ revs $ \bs ->
			case preRequisiteRef bs of
				Nothing -> S8.hPutStrLn h $
					fromRef' (includeRef bs)
				Just pr -> S8.hPutStrLn h $
					fromRef' pr
						<> ".." <>
					fromRef' (includeRef bs)
		hClose h

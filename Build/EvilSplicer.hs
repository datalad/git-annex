{- Expands template haskell splices
 -
 - First, the code must be built with a ghc that supports TH,
 - and the splices dumped to a log. For example:
 -   cabal build --ghc-options=-ddump-splices 2>&1 | tee log
 -
 - Along with the log, a headers file may also be provided, containing
 - additional imports needed by the template haskell code.
 -
 - This program will parse the log, and expand all splices therein,
 - writing files to the specified destdir (which can be "." to modify
 - the source tree directly). They can then be built a second
 - time, with a ghc that does not support TH.
 -
 - Note that template haskell code may refer to symbols that are not
 - exported by the library that defines the TH code. In this case,
 - the library has to be modifed to export those symbols.
 -
 - There can also be other problems with the generated code; it may
 - need modifications to compile.
 -
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>))
import Data.Either
import Data.List
import Data.String.Utils
import Data.Char
import System.Environment
import System.FilePath
import System.Directory
import Control.Monad

import Utility.Monad
import Utility.Misc
import Utility.Exception
import Utility.Path

data Coord = Coord
	{ coordLine :: Int
	, coordColumn :: Int
	}
	deriving (Read, Show)

offsetCoord :: Coord -> Coord -> Coord
offsetCoord a b = Coord
	(coordLine a - coordLine b)
	(coordColumn a - coordColumn b)

data SpliceType = SpliceExpression | SpliceDeclaration
	deriving (Read, Show, Eq)

data Splice = Splice
	{ splicedFile :: FilePath
	, spliceStart :: Coord
	, spliceEnd :: Coord
	, splicedExpression :: String
	, splicedCode :: String
	, spliceType :: SpliceType
	}
	deriving (Read, Show)

isExpressionSplice :: Splice -> Bool
isExpressionSplice s = spliceType s == SpliceExpression

number :: Parser Int
number = read <$> many1 digit

{- A pair of Coords is written in one of three ways:
 - "95:21-73", "1:1", or "(92,25)-(94,2)"
 -}
coordsParser :: Parser (Coord, Coord)
coordsParser = (try singleline <|> try weird <|> multiline) <?> "Coords"
  where
  	singleline = do
		line <- number
		char ':'
		startcol <- number
		char '-'
		endcol <- number
		return $ (Coord line startcol, Coord line endcol)

	weird = do
		line <- number
		char ':'
		col <- number
		return $ (Coord line col, Coord line col)

	multiline = do
		start <- fromparens
		char '-'
		end <- fromparens
		return $ (start, end)

	fromparens = between (char '(') (char ')') $ do
		line <- number
		char ','
		col <- number
		return $ Coord line col

indent :: Parser String
indent = many1 $ char ' '

restOfLine :: Parser String
restOfLine = newline `after` many (noneOf "\n")

indentedLine :: Parser String
indentedLine = indent >> restOfLine

spliceParser :: Parser Splice
spliceParser = do
	file <- many1 (noneOf ":\n")
	char ':'
	(start, end) <- coordsParser
	string ": Splicing "
	splicetype <- tosplicetype
		<$> (string "expression" <|> string "declarations")
	newline

	expression <- indentedLine

	indent
	string "======>"	
	newline

	{- All lines of the splice code will start with the same
	 - indent, which is stripped. Any other indentation is preserved. -}
	indent <- lookAhead indent
	let getcodeline = do
		string indent
		restOfLine

	{- When splicing declarations, GHC will output a splice
	 - at 1:1, and then inside the splice code block,
	 - the first line will give the actual coordinates of the
	 - line that was spliced.
	 -}
	let getrealcoords = do
		string indent
		string file
		char ':'
		char '\n' `after` coordsParser

	realcoords <- try (Right <$> getrealcoords) <|> (Left <$> getcodeline)
	codelines <- many getcodeline
	return $ case realcoords of
		Left firstcodeline -> 
			Splice file start end expression
				(unlines $ firstcodeline:codelines)
				splicetype
		Right (realstart, realend) ->
			Splice file realstart realend expression
				(unlines codelines)
				splicetype
  where
  	tosplicetype "declarations" = SpliceDeclaration
	tosplicetype "expression" = SpliceExpression
	tosplicetype s = error $ "unknown splice type: " ++ s

{- Extracts the splices, ignoring the rest of the compiler output. -}
splicesExtractor :: Parser [Splice]
splicesExtractor = rights <$> many extract
  where
  	extract = try (Right <$> spliceParser) <|> (Left <$> compilerJunkLine)
	compilerJunkLine = restOfLine

{- Modifies the source file, expanding the splices, which all must
 - have the same splicedFile. Writes the new file to the destdir.
 -
 - Each splice's Coords refer to the original position in the file,
 - and not to its position after any previous splices may have inserted
 - or removed lines.
 -
 - To deal with this complication, the file is broken into logical lines
 - (which can contain any String, including a multiline or empty string).
 - Each splice is assumed to be on its own block of lines; two
 - splices on the same line is not currently supported.
 - This means that a splice can modify the logical lines within its block
 - as it likes, without interfering with the Coords of other splices.
 - 
 - When splicing in declarations, they are not placed on the line
 - that defined them, because at least with Yesod, that line has another TH
 - splice, and things would get mixed up. Since declarations are stand
 - alone, they can go anywhere, and are added to the very end of the file.
 -
 - As well as expanding splices, this can add a block of imports to the
 - file. These are put right before the first line in the file that
 - starts with "import "
 -}
applySplices :: FilePath -> Maybe String -> [Splice] -> IO ()
applySplices destdir imports splices@(first:_) = do
	let f = splicedFile first
	let dest = (destdir </> f)
	putStrLn $ "splicing " ++ f
	lls <- map (++ "\n") . lines <$> readFileStrict f
	createDirectoryIfMissing True (parentDir dest)
	let newcontent = concat $ addimports $
		expanddeclarations declarationsplices $
		expandexpressions lls expressionsplices
	oldcontent <- catchMaybeIO $ readFileStrict dest
	when (oldcontent /= Just newcontent) $
		writeFile dest newcontent
  where
	(expressionsplices, declarationsplices) =
		partition isExpressionSplice splices

  	expandexpressions lls [] = lls
  	expandexpressions lls (s:rest) =
		expandexpressions (expandExpressionSplice s lls) rest

	expanddeclarations [] lls = lls
	expanddeclarations l lls = lls ++ map (mangleCode . splicedCode) l

	addimports lls = case imports of
		Nothing -> lls
		Just v ->
			let (start, end) = break ("import " `isPrefixOf`) lls
			in if null end
				then start
				else concat
					[ start
					, [v]
					, end
					]

expandExpressionSplice :: Splice -> [String] -> [String]
expandExpressionSplice s lls = concat [before, new:splicerest, end]
  where
	cs = spliceStart s
	ce = spliceEnd s

	(before, rest) = splitAt (coordLine cs - 1) lls
	(oldlines, end) = splitAt (1 + coordLine (offsetCoord ce cs)) rest
	(splicestart, splicerest) = case oldlines of
		l:r -> (expandtabs l, take (length r) (repeat []))
		_ -> ([], [])
	new = concat
		[ joinsplice $ deqqstart $ take (coordColumn cs - 1) splicestart
		, addindent (findindent splicestart) (mangleCode $ splicedCode s)
		, deqqend $ drop (coordColumn ce) splicestart
		]

	{- coordinates assume tabs are expanded to 8 spaces -}
	expandtabs = replace "\t" (take 8 $ repeat ' ')

	{- splicing leaves $() quasiquote behind; remove it -}
	deqqstart s = case reverse s of
		('(':'$':rest) -> reverse rest
		_ -> s
	deqqend (')':s) = s
	deqqend s = s

	{- Prepare the code that comes just before the splice so
	 - the splice will combine with it appropriately. -}
	joinsplice s
		-- all indentation? Skip it, we'll use the splice's indentation
		| all isSpace s = ""
		-- function definition needs no preparation
		-- ie: foo = $(splice)
		| "=" `isSuffixOf` s' = s
		-- already have a $ to set off the splice
		-- ie: foo $ $(splice)
		| "$" `isSuffixOf` s' = s
		-- need to add a $ to set off the splice
		-- ie: bar $(splice)
		| otherwise = s ++ " $ "
	  where
	  	s' = filter (not . isSpace) s

	findindent = length . takeWhile isSpace
	addindent n = unlines . map (i ++) . lines
	  where
	  	i = take n $ repeat ' '

{- Tweaks code output by GHC in splices to actually build. Yipes. -}
mangleCode :: String -> String
mangleCode = fix_bad_escape . remove_package_version
  where
	{- GHC may incorrectly escape "}" within a multi-line string. -}
	fix_bad_escape = replace " \\}" " }"

	{- GHC may add full package and version qualifications for
	 - symbols from unimported modules. We don't want these.
	 -
	 - Examples:
	 -   "blaze-html-0.4.3.1:Text.Blaze.Internal.preEscapedText" 
	 -   "ghc-prim:GHC.Types.:"
	 -}
	remove_package_version s = case parse findQualifiedSymbols "" s of
		Left e -> s
		Right symbols -> concat $ 
			map (either (\c -> [c]) mangleSymbol) symbols

	findQualifiedSymbols :: Parser [Either Char String]
	findQualifiedSymbols = many $
		try (Right <$> qualifiedSymbol) <|> (Left <$> anyChar)

	qualifiedSymbol :: Parser String
	qualifiedSymbol = do
		token
		char ':'
		token

	token :: Parser String
	token = many1 $ satisfy isAlphaNum <|> oneOf "-.'"

	mangleSymbol "GHC.Types." = ""
	mangleSymbol s = s

main :: IO ()
main = go =<< getArgs
  where
	go (destdir:log:header:[]) = run destdir log (Just header)
	go (destdir:log:[]) = run destdir log Nothing
  	go _ = error "usage: EvilSplicer destdir logfile [headerfile]"

	run destdir log mheader = do
		r <- parseFromFile splicesExtractor log
		case r of
			Left e -> error $ show e
			Right splices -> do
				let groups = groupBy (\a b -> splicedFile a == splicedFile b) splices
				imports <- maybe (return Nothing) (catchMaybeIO . readFile) mheader
				mapM_ (applySplices destdir imports) groups

{- Expands template haskell splices
 -
 - You should probably just use http://hackage.haskell.org/package/zeroth
 - instead. I wish I had known about it before writing this.
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
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Main where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>))
import Data.Either
import Data.List hiding (find)
import Data.String.Utils
import Data.Char
import System.Environment
import System.FilePath
import System.IO
import Control.Monad
import Prelude hiding (log)

import Utility.Monad
import Utility.Misc
import Utility.Exception hiding (try)
import Utility.Path
import Utility.FileSystemEncoding
import Utility.Directory

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
		void $ char ':'
		startcol <- number
		void $ char '-'
		endcol <- number
		return $ (Coord line startcol, Coord line endcol)

	weird = do
		line <- number
		void $ char ':'
		col <- number
		return $ (Coord line col, Coord line col)

	multiline = do
		start <- fromparens
		void $ char '-'
		end <- fromparens
		return $ (start, end)

	fromparens = between (char '(') (char ')') $ do
		line <- number
		void $ char ','
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
	void $ char ':'
	(start, end) <- coordsParser
	void $ string ": Splicing "
	splicetype <- tosplicetype
		<$> (string "expression" <|> string "declarations")
	void newline

	getthline <- expressionextractor
	expression <- unlines <$> many1 getthline

	void indent
	void $ string "======>"	
	void newline

	getcodeline <- expressionextractor
	realcoords <- try (Right <$> getrealcoords file) <|> (Left <$> getcodeline)
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

	{- All lines of the indented expression start with the same
	 - indent, which is stripped. Any other indentation is preserved. -}
	expressionextractor = do
		i <- lookAhead indent
		return $ try $ do
			void $ string i
			restOfLine
	
	{- When splicing declarations, GHC will output a splice
	 - at 1:1, and then inside the splice code block,
	 - the first line will give the actual coordinates of the
	 - line that was spliced. -}
	getrealcoords file = do
		void indent
		void $ string file
		void $ char ':'
		char '\n' `after` coordsParser

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
 - As well as expanding splices, this can add a block of imports to the
 - file. These are put right before the first line in the file that
 - starts with "import "
 -}
applySplices :: FilePath -> Maybe String -> [Splice] -> IO ()
applySplices _ _ [] = noop
applySplices destdir imports splices@(first:_) = do
	let f = splicedFile first
	let dest = (destdir </> f)
	lls <- map (++ "\n") . lines <$> readFileStrictAnyEncoding f
	createDirectoryIfMissing True (parentDir dest)
	let newcontent = concat $ addimports $ expand lls splices
	oldcontent <- catchMaybeIO $ readFileStrictAnyEncoding dest
	when (oldcontent /= Just newcontent) $ do
		putStrLn $ "splicing " ++ f
		withFile dest WriteMode $ \h -> do
		        fileEncoding h
			hPutStr h newcontent
		        hClose h
  where
	expand lls [] = lls
	expand lls (s:rest)
		| isExpressionSplice s = expand (expandExpressionSplice s lls) rest
		| otherwise = expand (expandDeclarationSplice s lls) rest

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

{- Declaration splices are expanded to replace their whole line. -}
expandDeclarationSplice :: Splice -> [String] -> [String]
expandDeclarationSplice s lls = concat [before, [splice], end]
  where
	cs = spliceStart s
	ce = spliceEnd s

	(before, rest) = splitAt (coordLine cs - 1) lls
	(_oldlines, end) = splitAt (1 + coordLine (offsetCoord ce cs)) rest
	splice = mangleCode $ splicedCode s

{- Expression splices are expanded within their line. -}
expandExpressionSplice :: Splice -> [String] -> [String]
expandExpressionSplice sp lls = concat [before, spliced:padding, end]
  where
	cs = spliceStart sp
	ce = spliceEnd sp

	(before, rest) = splitAt (coordLine cs - 1) lls
	(oldlines, end) = splitAt (1 + coordLine (offsetCoord ce cs)) rest
	(splicestart, padding, spliceend) = case map expandtabs oldlines of
		ss:r
			| null r -> (ss, [], ss)
			| otherwise -> (ss, take (length r) (repeat []), last r)
		_ -> ([], [], [])
	spliced = concat
		[ joinsplice $ deqqstart $ take (coordColumn cs - 1) splicestart
		, addindent (findindent splicestart) (mangleCode $ splicedCode sp)
		, deqqend $ drop (coordColumn ce) spliceend
		]

	{- coordinates assume tabs are expanded to 8 spaces -}
	expandtabs = replace "\t" (take 8 $ repeat ' ')

	{- splicing leaves $() quasiquote behind; remove it -}
	deqqstart s = case reverse s of
		('(':'$':restq) -> reverse restq
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
		-- nor does lambda definition or case expression
		| "->" `isSuffixOf` s' = s
		-- nor does a let .. in declaration
		| "in" `isSuffixOf` s' = s
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
mangleCode = flip_colon
	. persist_dequalify_hack
	. let_do
	. remove_unnecessary_type_signatures
	. lambdaparenhackyesod
	. lambdaparenhackpersistent
	. lambdaparens
	. declaration_parens
	. case_layout
	. case_layout_multiline
	. yesod_url_render_hack
	. text_builder_hack
	. nested_instances 
	. boxed_fileembed
	. collapse_multiline_strings
	. remove_package_version
	. emptylambda
  where
	{- Lambdas are often output without parens around them.
	 - This breaks when the lambda is immediately applied to a
	 - parameter.
	 - 
	 - For example:
	 -
	 - renderRoute (StaticR sub_a1nUH)
	 -   = \ (a_a1nUI, b_a1nUJ)
	 -       -> (((pack "static") : a_a1nUI),
	 -            b_a1nUJ)
	 -       (renderRoute sub_a1nUH)
	 -
	 - There are sometimes many lines of lambda code that need to be
	 - parenthesised. Approach: find the "->" and scan down the
	 - column to the first non-whitespace. This is assumed
	 - to be the expression after the lambda.
	 -
	 - Runs recursively on the body of the lambda, to handle nested
	 - lambdas.
	 -}
	lambdaparens = parsecAndReplace $ do
		-- skip lambdas inside tuples or parens
		prefix <- noneOf "(, \n"
		preindent <- many1 $ oneOf " \n"
		void $ string "\\ "
		lambdaparams <- restofline
		continuedlambdaparams <- many $ try $ do
			indent1 <- many1 $ char ' '
			p <- satisfy isLetter
			aram <- many $ satisfy isAlphaNum <|> oneOf "_"
			void newline
			return $ indent1 ++ p:aram ++ "\n"
		indent1 <- many1 $ char ' '
		void $ string "-> "
		firstline <- restofline
		lambdalines <- many $ try $ do
			void $ string indent1
			void $ char ' '
			l <- restofline
			return $ indent1 ++ " " ++ l
		return $ concat 
			[ prefix:preindent
			, "(\\ " ++ lambdaparams ++ "\n"
			, concat continuedlambdaparams
			, indent1 ++ "-> "
			, lambdaparens $ intercalate "\n" (firstline:lambdalines)
			, ")\n"
			]
	
	{- Hack to add missing parens in a specific case in yesod
	 - static route code.
	 -
	 -     StaticR
	 -     yesod_dispatch_env_a4iDV
	 -     (\ p_a4iE2 r_a4iE3
	 -        -> r_a4iE3
	 -          {Network.Wai.pathInfo = p_a4iE2}
	 -        xrest_a4iDT req_a4iDW)) }
	 -
	 - Need to add another paren around the lambda, and close it
	 - before its parameters. lambdaparens misses this one because
	 - there is already one paren present.
	 -
	 - Note that the { } may be on the same line, or wrapped to next.
	 -
	 - FIXME: This is a hack. lambdaparens could just always add a
	 - layer of parens even when a lambda seems to be in parent.
	 -}
	lambdaparenhackyesod = parsecAndReplace $ do
		indent1 <- many1 $ char ' '
		staticr <- string "StaticR"
		void newline
		void $ string indent1
		yesod_dispatch_env <- restofline
		void $ string indent1
		lambdaprefix <- string "(\\ "
		l1 <- restofline
		void $ string indent1
		lambdaarrow <- string "   ->"
		l2 <- restofline
		l3 <- if '{' `elem` l2 && '}' `elem` l2
			then return ""
			else do
				void $ string indent1
				restofline
		return $ unlines
			[ indent1 ++ staticr
			, indent1 ++ yesod_dispatch_env
			, indent1 ++ "(" ++ lambdaprefix ++ l1
			, indent1 ++ lambdaarrow ++ l2 ++ l3 ++ ")"
			]

	{- Hack to reorder misplaced paren in persistent code.
	 -
	 - = ((Right Fscked)
         -    (\ persistValue_a36iM
         -       -> case fromPersistValue persistValue_a36iM of {
         -            Right r_a36iN -> Right r_a36iN
         -            Left err_a36iO
         -              -> (Left
         -                  $ ((("field " `Data.Monoid.mappend` (packPTH "key"))
         -                      `Data.Monoid.mappend` ": ")
         -                     `Data.Monoid.mappend` err_a36iO)) }
         -       x_a36iL))
	 -
	 - Fixed by adding another level of params around the lambda
	 - (lambdaparams should be generalized to cover this case).
	 -}
	lambdaparenhackpersistent = parsecAndReplace $ do
		indent1 <- many1 $ char ' '
		start <- do
			s1 <- string "(\\ "
			s2 <- string "persistValue_"
			s3 <- restofline
			return $ s1 ++ s2 ++ s3
		void $ string indent1
		indent2 <- many1 $ char ' '
		void $ string "-> "
		l1 <- restofline
		lambdalines <- many $ try $ do
			void $ string $ indent1 ++ indent2 ++ " "
			l <- restofline
			return $ indent1 ++ indent2 ++ " " ++ l
		return $ concat
			[ indent1 ++ "(" ++ start ++ "\n"
			, indent1 ++ indent2 ++ "-> " ++ l1 ++ "\n"
			, intercalate "\n" lambdalines
			, ")\n"
			]

	restofline = manyTill (noneOf "\n") newline

	{- For some reason, GHC sometimes doesn't like the multiline
	 - strings it creates. It seems to get hung up on \{ at the
	 - start of a new line sometimes, wanting it to not be escaped.
	 -
	 - To work around what is likely a GHC bug, just collapse
	 - multiline strings. -}
	collapse_multiline_strings = parsecAndReplace $ do
		void $ string "\\\n"
		void $ many1 $ oneOf " \t"
		void $ string "\\"
		return "\\n"

	{- GHC outputs splices using explicit braces rather than layout.
	 - For a case expression, it does something weird:
	 -
	 - case foo of {
	 -   xxx -> blah
	 -   yyy -> blah };
	 -
	 - This is not legal Haskell; the statements in the case must be
	 - separated by ';'
	 -
	 - To fix, we could just put a semicolon at the start of every line
	 - containing " -> " ... Except that lambdas also contain that.
	 - But we can get around that: GHC outputs lambas like this:
	 -
	 - \ foo
	 -   -> bar
	 -
	 - Or like this:
	 -
	 - \ foo -> bar
	 -
	 - So, we can put the semicolon at the start of every line
	 - containing " -> " unless there's a "\ " first, or it's
	 - all whitespace up until it.
	 -}
	case_layout = parsecAndReplace $ do
		void newline
		indent1 <- many1 $ char ' '
		prefix <- manyTill (noneOf "\n") (try (string "-> "))
		if length prefix > 20
			then unexpected "too long a prefix"
			else if "\\ " `isInfixOf` prefix
				then unexpected "lambda expression"
				else if null prefix
					then unexpected "second line of lambda"
					else return $ "\n" ++ indent1 ++ "; " ++ prefix ++ " -> "
	{- Sometimes cases themselves span multiple lines:
	 -
	 - Nothing
	 -   -> foo
	 -
	 - -- This is not yet handled!
	 - ComplexConstructor  var var
	 -        var var
	 -   -> foo
	 -}
	case_layout_multiline = parsecAndReplace $ do
		void newline
		indent1 <- many1 $ char ' '
		firstline <- restofline

		void $ string indent1
		indent2 <- many1 $ char ' '
		void $ string "-> "
		if "\\ " `isInfixOf` firstline
			then unexpected "lambda expression"
			else return $ "\n" ++ indent1 ++ "; " ++ firstline ++ "\n"
				++ indent1 ++ indent2 ++ "-> "

	{- (foo, \ -> bar) is not valid haskell, GHC.
	 - Change to (foo, bar)
	 -
	 - (Does this ever happen outside a tuple? Only saw
	 - it inside them..
	 -}
	emptylambda = replace ", \\ -> " ", "

	{- GHC may output this:
	 -
	 - instance RenderRoute WebApp where
	 -   data instance Route WebApp
	 -        ^^^^^^^^
	 - The marked word should not be there.
	 -
	 - FIXME: This is a yesod and persistent-specific hack,
	 - it should look for the outer instance.
	 -}
	nested_instances = replace "  data instance Route" "  data Route"
		. replace "  data instance Unique" "  data Unique"
		. replace "  data instance EntityField" "  data EntityField"
		. replace "  type instance PersistEntityBackend" "  type PersistEntityBackend"

	{- GHC does not properly parenthesise generated data type
	 - declarations. -}
	declaration_parens = replace "StaticR Route Static" "StaticR (Route Static)"

	{- A type signature is sometimes given for an entire lambda,
	 - which is not properly parenthesized or laid out. This is a
	 - hack to remove one specific case where this happens and the
	 - signature is easily inferred, so is just removed.
	 -}
	remove_unnecessary_type_signatures = parsecAndReplace $ do
		void $ string " ::"
		void newline
		void $ many1 $ char ' '
		void $ string "Text.Css.Block Text.Css.Resolved"
		void newline
		return ""

	{- GHC may add full package and version qualifications for
	 - symbols from unimported modules. We don't want these.
	 -
	 - Examples:
	 -   "blaze-html-0.4.3.1:Text.Blaze.Internal.preEscapedText" 
	 -   "ghc-prim:GHC.Types.:"
	 -}
	remove_package_version = parsecAndReplace $
		mangleSymbol <$> qualifiedSymbol

	mangleSymbol "GHC.Types." = ""
	mangleSymbol "GHC.Tuple." = ""
	mangleSymbol s = s

	qualifiedSymbol :: Parser String
	qualifiedSymbol = do
		s <- hstoken
		void $ char ':'
		if length s < 5
			then unexpected "too short to be a namespace"
			else do
				t <- hstoken
				case t of
					(c:r) | isUpper c && "." `isInfixOf` r -> return t
					_ -> unexpected "not a module qualified symbol"

	hstoken :: Parser String
	hstoken = do
		t <- satisfy isLetter
		oken <- many $ satisfy isAlphaNum <|> oneOf "-.'"
		return $ t:oken

	{- This works when it's "GHC.Types.:", but we strip
	 - that above, so have to fix up after it here. 
	 - The ; is added by case_layout. -}
	flip_colon = replace "; : _ " "; _ : "

	{- TH for persistent has some qualified symbols in places
	 - that are not allowed. -}
	persist_dequalify_hack = replace "Database.Persist.TH.++" "`Data.Text.append`"
		. replace "Database.Persist.Sql.Class.sqlType" "sqlType"
		. replace "Database.Persist.Class.PersistField.toPersistValue" "toPersistValue"
		. replace "Database.Persist.Class.PersistField.fromPersistValue" "fromPersistValue"

	{- Sometimes generates invalid bracketed code with a let
	 - expression:
	 -
	 - foo = do { let x = foo;
	 -            use foo }
	 -
	 - Fix by converting the "let x = " to "x <- return $"
	 -}
	let_do = parsecAndReplace $ do
		void $ string "= do { let "
		x <- many $ noneOf "=\r\n"
		_ <- many1 $ oneOf " \t\r\n"
		void $ string "= "
		return $ "= do { " ++ x ++ " <- return $ "

{- Embedded files use unsafe packing, which is problimatic
 - for several reasons, including that GHC sometimes omits trailing
 - newlines in the file content, which leads to the wrong byte
 - count. Also, GHC sometimes outputs unicode characters, which 
 - are not legal in unboxed strings. 
 -
 - Avoid problems by converting:
 - GHC.IO.unsafePerformIO
 -   (Data.ByteString.Unsafe.unsafePackAddressLen
 -      lllll
 -      "blabblah"#)),
 - to:
 - Data.ByteString.Char8.pack "blabblah"),
 -
 - Note that the string is often multiline. This only works if
 - collapse_multiline_strings has run first.
 -}
boxed_fileembed :: String -> String
boxed_fileembed = parsecAndReplace $ do
	i <- indent
	void $ string "GHC.IO.unsafePerformIO"
	void newline
	void indent
	void $ string "(Data.ByteString.Unsafe.unsafePackAddressLen"
	void newline
	void indent
	void number
	void newline
	void indent
	void $ char '"'
	s <- restOfLine
	let s' = take (length s - 5) s
	if "\"#))," `isSuffixOf` s
		then return (i ++ "Data.ByteString.Char8.pack \"" ++ s' ++ "\"),\n")
		else fail "not an unboxed string"

{- This works around a problem in the expanded template haskell for Yesod
 - type-safe url rendering.
 -
 - It generates code like this:
 - 
 -                                  (toHtml
 -                                     (\ u_a2ehE -> urender_a2ehD u_a2ehE []
 -                                        (CloseAlert aid)))));
 -
 - Where urender_a2ehD is the function returned by getUrlRenderParams.
 - But, that function that only takes 2 params, not 3.
 - And toHtml doesn't take a parameter at all!
 - 
 - So, this modifes the code, to look like this:
 - 
 -                                  (toHtml
 -                                     (flip urender_a2ehD []
 -                                        (CloseAlert aid)))));
 - 
 - FIXME: Investigate and fix this properly.
 -}
yesod_url_render_hack :: String -> String
yesod_url_render_hack = parsecAndReplace $ do
	void $ string "(toHtml"
	void whitespace
	void $ string "(\\"
	void whitespace
	wtf <- hstoken
	void whitespace
	void $ string "->"
	void whitespace
	renderer <- hstoken
	void whitespace
	void $ string wtf
	void whitespace
	return $ "(toHtml (flip " ++ renderer ++ " "
  where
	whitespace :: Parser String
	whitespace = many $ oneOf " \t\r\n"

	hstoken :: Parser String
	hstoken = many1 $ satisfy isAlphaNum <|> oneOf "_"

{- Use exported symbol. -}
text_builder_hack :: String -> String
text_builder_hack = replace "Data.Text.Lazy.Builder.Internal.fromText" "Data.Text.Lazy.Builder.fromText"

{- Given a Parser that finds strings it wants to modify,
 - and returns the modified string, does a mass 
 - find and replace throughout the input string.
 - Rather slow, but crazy powerful. -}
parsecAndReplace :: Parser String -> String -> String
parsecAndReplace p s = case parse find "" s of
	Left _e -> s
	Right l -> concatMap (either return id) l
  where
	find :: Parser [Either Char String]
	find = many $ try (Right <$> p) <|> (Left <$> anyChar)

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

-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Lexer
-- Copyright   :  (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Lexer for Haskell.
--
-----------------------------------------------------------------------------

-- ToDo: Introduce different tokens for decimal, octal and hexadecimal (?)
-- ToDo: FloatTok should have three parts (integer part, fraction, exponent) (?)
-- ToDo: Use a lexical analyser generator (lx?)

module Language.Haskell.Lexer (Token(..), lexer) where

import Language.Haskell.ParseMonad

import Data.Char	(isAlpha, isLower, isUpper, toLower,
			 isDigit, isHexDigit, isOctDigit, isSpace,
			 ord, chr, digitToInt)
import Data.Ratio

data Token
        = VarId String
        | QVarId (String,String)
	| ConId String
        | QConId (String,String)
        | VarSym String
        | ConSym String
        | QVarSym (String,String)
        | QConSym (String,String)
	| IntTok Integer
	| FloatTok Rational
	| Character Char
        | StringTok String

-- Symbols

	| LeftParen
	| RightParen
	| SemiColon
        | LeftCurly
        | RightCurly
        | VRightCurly			-- a virtual close brace
        | LeftSquare
        | RightSquare
	| Comma
        | Underscore
        | BackQuote

-- Reserved operators

	| DotDot
	| Colon
	| DoubleColon
	| Equals
	| Backslash
	| Bar
	| LeftArrow
	| RightArrow
	| At
	| Tilde
	| DoubleArrow
	| Minus
	| Exclamation

-- Reserved Ids

	| KW_Case
	| KW_Class
	| KW_Data
	| KW_Default
	| KW_Deriving
	| KW_Do
	| KW_Else
	| KW_Foreign
	| KW_If
	| KW_Import
	| KW_In
	| KW_Infix
	| KW_InfixL
	| KW_InfixR
	| KW_Instance
	| KW_Let
	| KW_Module
	| KW_NewType
	| KW_Of
	| KW_Then
	| KW_Type
	| KW_Where

-- Special Ids

	| KW_As
	| KW_Export
	| KW_Hiding
	| KW_Qualified
	| KW_Safe
	| KW_Unsafe

        | EOF
        deriving (Eq,Show)

reserved_ops :: [(String,Token)]
reserved_ops = [
 ( "..", DotDot ),
 ( ":",  Colon ),
 ( "::", DoubleColon ),
 ( "=",  Equals ),
 ( "\\", Backslash ),
 ( "|",  Bar ),
 ( "<-", LeftArrow ),
 ( "->", RightArrow ),
 ( "@",  At ),
 ( "~",  Tilde ),
 ( "=>", DoubleArrow )
 ]

special_varops :: [(String,Token)]
special_varops = [
 ( "-",  Minus ),			--ToDo: shouldn't be here
 ( "!",  Exclamation )		--ditto
 ]

reserved_ids :: [(String,Token)]
reserved_ids = [
 ( "_",         Underscore ),
 ( "case",      KW_Case ),
 ( "class",     KW_Class ),
 ( "data",      KW_Data ),
 ( "default",   KW_Default ),
 ( "deriving",  KW_Deriving ),
 ( "do",        KW_Do ),
 ( "else",      KW_Else ),
 ( "foreign",	KW_Foreign ),
 ( "if",    	KW_If ),
 ( "import",    KW_Import ),
 ( "in", 	KW_In ),
 ( "infix", 	KW_Infix ),
 ( "infixl", 	KW_InfixL ),
 ( "infixr", 	KW_InfixR ),
 ( "instance",  KW_Instance ),
 ( "let", 	KW_Let ),
 ( "module", 	KW_Module ),
 ( "newtype",   KW_NewType ),
 ( "of", 	KW_Of ),
 ( "then", 	KW_Then ),
 ( "type", 	KW_Type ),
 ( "where", 	KW_Where )
 ]

special_varids :: [(String,Token)]
special_varids = [
 ( "as", 	KW_As ),
 ( "export", 	KW_Export ),
 ( "hiding", 	KW_Hiding ),
 ( "qualified", KW_Qualified ),
 ( "safe",	KW_Safe ),
 ( "unsafe", 	KW_Unsafe )
 ]

isIdent, isSymbol :: Char -> Bool
isIdent  c = isAlpha c || isDigit c || c == '\'' || c == '_'
isSymbol c = elem c ":!#$%&*+./<=>?@\\^|-~"

matchChar :: Char -> String -> Lex a ()
matchChar c msg = do
	s <- getInput
	if null s || head s /= c then fail msg else discard 1

-- The top-level lexer.
-- We need to know whether we are at the beginning of the line to decide
-- whether to insert layout tokens.

lexer :: (Token -> P a) -> P a
lexer = runL $ do
	bol <- checkBOL
	bol <- lexWhiteSpace bol
	startToken
	if bol then lexBOL else lexToken

lexWhiteSpace :: Bool -> Lex a Bool
lexWhiteSpace bol = do
	s <- getInput
	case s of
	    '{':'-':_ -> do
		discard 2
		bol <- lexNestedComment bol
		lexWhiteSpace bol
	    '-':'-':rest | all (== '-') (takeWhile isSymbol rest) -> do
		lexWhile (== '-')
		lexWhile (/= '\n')
		s' <- getInput
		case s' of
		    [] -> fail "Unterminated end-of-line comment"
		    _ -> do
			lexNewline
			lexWhiteSpace True
	    '\n':_ -> do
		lexNewline
		lexWhiteSpace True
	    '\t':_ -> do
		lexTab
		lexWhiteSpace bol
	    c:_ | isSpace c -> do
		discard 1
		lexWhiteSpace bol
	    _ -> return bol

lexNestedComment :: Bool -> Lex a Bool
lexNestedComment bol = do
	s <- getInput
	case s of
	    '-':'}':_ -> discard 2 >> return bol
	    '{':'-':_ -> do
		discard 2
		bol <- lexNestedComment bol	-- rest of the subcomment
		lexNestedComment bol		-- rest of this comment
	    '\t':_    -> lexTab >> lexNestedComment bol
	    '\n':_    -> lexNewline >> lexNestedComment True
	    _:_       -> discard 1 >> lexNestedComment bol
	    []        -> fail "Unterminated nested comment"

-- When we are lexing the first token of a line, check whether we need to
-- insert virtual semicolons or close braces due to layout.

lexBOL :: Lex a Token
lexBOL = do
	pos <- getOffside
	case pos of
	    LT -> do
                -- trace "layout: inserting '}'\n" $
        	-- Set col to 0, indicating that we're still at the
        	-- beginning of the line, in case we need a semi-colon too.
        	-- Also pop the context here, so that we don't insert
        	-- another close brace before the parser can pop it.
		setBOL
		popContextL "lexBOL"
		return VRightCurly
	    EQ ->
                -- trace "layout: inserting ';'\n" $
		return SemiColon
	    GT ->
		lexToken

lexToken :: Lex a Token
lexToken = do
    s <- getInput
    case s of
        [] -> return EOF

	'0':c:d:_ | toLower c == 'o' && isOctDigit d -> do
			discard 2
			n <- lexOctal
			return (IntTok n)
		  | toLower c == 'x' && isHexDigit d -> do
			discard 2
			n <- lexHexadecimal
			return (IntTok n)

	c:_ | isDigit c -> lexDecimalOrFloat

	    | isUpper c -> lexConIdOrQual ""

	    | isLower c || c == '_' -> do
		ident <- lexWhile isIdent
		return $ case lookup ident (reserved_ids ++ special_varids) of
			Just keyword -> keyword
			Nothing -> VarId ident

	    | isSymbol c -> do
		sym <- lexWhile isSymbol
		return $ case lookup sym (reserved_ops ++ special_varops) of
			Just t  -> t
			Nothing -> case c of
			    ':' -> ConSym sym
			    _   -> VarSym sym

	    | otherwise -> do
		discard 1
		case c of

		    -- First the special symbols
		    '(' ->  return LeftParen
		    ')' ->  return RightParen
		    ',' ->  return Comma
		    ';' ->  return SemiColon
		    '[' ->  return LeftSquare
		    ']' ->  return RightSquare
		    '`' ->  return BackQuote
		    '{' -> do
			    pushContextL NoLayout
			    return LeftCurly
		    '}' -> do
			    popContextL "lexToken"
			    return RightCurly

		    '\'' -> do
			    c2 <- lexChar
			    matchChar '\'' "Improperly terminated character constant"
			    return (Character c2)

		    '"' ->  lexString

		    _ ->    fail ("Illegal character \'" ++ show c ++ "\'\n")

lexDecimalOrFloat :: Lex a Token
lexDecimalOrFloat = do
	ds <- lexWhile isDigit
	rest <- getInput
	case rest of
	    ('.':d:_) | isDigit d -> do
		discard 1
		frac <- lexWhile isDigit
		let num = parseInteger 10 (ds ++ frac)
		    decimals = toInteger (length frac)
		exponent <- do
			rest2 <- getInput
			case rest2 of
			    'e':_ -> lexExponent
			    'E':_ -> lexExponent
			    _     -> return 0
		return (FloatTok ((num%1) * 10^^(exponent - decimals)))
	    e:_ | toLower e == 'e' -> do
		exponent <- lexExponent
		return (FloatTok ((parseInteger 10 ds%1) * 10^^exponent))
	    _ -> return (IntTok (parseInteger 10 ds))

    where
	lexExponent :: Lex a Integer
	lexExponent = do
		discard 1	-- 'e' or 'E'
		r <- getInput
		case r of
		    '+':d:_ | isDigit d -> do
			discard 1
			lexDecimal
		    '-':d:_ | isDigit d -> do
			discard 1
			n <- lexDecimal
			return (negate n)
		    d:_ | isDigit d -> lexDecimal
		    _ -> fail "Float with missing exponent"

lexConIdOrQual :: String -> Lex a Token
lexConIdOrQual qual = do
	con <- lexWhile isIdent
	let conid | null qual = ConId con
		  | otherwise = QConId (qual,con)
	    qual' | null qual = con
		  | otherwise = qual ++ '.':con
	just_a_conid <- alternative (return conid)
	rest <- getInput
	case rest of
	  '.':c:_
	     | isLower c || c == '_' -> do	-- qualified varid?
		discard 1
		ident <- lexWhile isIdent
		case lookup ident reserved_ids of
		   -- cannot qualify a reserved word
		   Just _  -> just_a_conid
		   Nothing -> return (QVarId (qual', ident))

	     | isUpper c -> do		-- qualified conid?
		discard 1
		lexConIdOrQual qual'

	     | isSymbol c -> do	-- qualified symbol?
		discard 1
		sym <- lexWhile isSymbol
		case lookup sym reserved_ops of
		    -- cannot qualify a reserved operator
		    Just _  -> just_a_conid
		    Nothing -> return $ case c of
			':' -> QConSym (qual', sym)
			_   -> QVarSym (qual', sym)

	  _ ->	return conid -- not a qualified thing

lexChar :: Lex a Char
lexChar = do
	r <- getInput
	case r of
		'\\':_	-> lexEscape
		c:_	-> discard 1 >> return c
		[]	-> fail "Incomplete character constant"

lexString :: Lex a Token
lexString = loop ""
    where
	loop s = do
		r <- getInput
		case r of
		    '\\':'&':_ -> do
				discard 2
				loop s
		    '\\':c:_ | isSpace c -> do
				discard 1
				lexWhiteChars
				matchChar '\\' "Illegal character in string gap"
				loop s
			     | otherwise -> do
				ce <- lexEscape
				loop (ce:s)
		    '"':_ -> do
				discard 1
				return (StringTok (reverse s))
		    c:_ -> do
				discard 1
				loop (c:s)
		    [] ->	fail "Improperly terminated string"

	lexWhiteChars :: Lex a ()
	lexWhiteChars = do
		s <- getInput
		case s of
		    '\n':_ -> do
			lexNewline
			lexWhiteChars
		    '\t':_ -> do
			lexTab
			lexWhiteChars
		    c:_ | isSpace c -> do
			discard 1
			lexWhiteChars
		    _ -> return ()

lexEscape :: Lex a Char
lexEscape = do
	discard 1
	r <- getInput
	case r of

-- Production charesc from section B.2 (Note: \& is handled by caller)

		'a':_		-> discard 1 >> return '\a'
		'b':_		-> discard 1 >> return '\b'
		'f':_		-> discard 1 >> return '\f'
		'n':_		-> discard 1 >> return '\n'
		'r':_		-> discard 1 >> return '\r'
		't':_		-> discard 1 >> return '\t'
		'v':_		-> discard 1 >> return '\v'
		'\\':_		-> discard 1 >> return '\\'
		'"':_		-> discard 1 >> return '\"'
		'\'':_		-> discard 1 >> return '\''

-- Production ascii from section B.2

		'^':c:_		-> discard 2 >> cntrl c
		'N':'U':'L':_	-> discard 3 >> return '\NUL'
		'S':'O':'H':_	-> discard 3 >> return '\SOH'
		'S':'T':'X':_	-> discard 3 >> return '\STX'
		'E':'T':'X':_	-> discard 3 >> return '\ETX'
		'E':'O':'T':_	-> discard 3 >> return '\EOT'
		'E':'N':'Q':_	-> discard 3 >> return '\ENQ'
		'A':'C':'K':_	-> discard 3 >> return '\ACK'
		'B':'E':'L':_	-> discard 3 >> return '\BEL'
		'B':'S':_	-> discard 2 >> return '\BS'
		'H':'T':_	-> discard 2 >> return '\HT'
		'L':'F':_	-> discard 2 >> return '\LF'
		'V':'T':_	-> discard 2 >> return '\VT'
		'F':'F':_	-> discard 2 >> return '\FF'
		'C':'R':_	-> discard 2 >> return '\CR'
		'S':'O':_	-> discard 2 >> return '\SO'
		'S':'I':_	-> discard 2 >> return '\SI'
		'D':'L':'E':_	-> discard 3 >> return '\DLE'
		'D':'C':'1':_	-> discard 3 >> return '\DC1'
		'D':'C':'2':_	-> discard 3 >> return '\DC2'
		'D':'C':'3':_	-> discard 3 >> return '\DC3'
		'D':'C':'4':_	-> discard 3 >> return '\DC4'
		'N':'A':'K':_	-> discard 3 >> return '\NAK'
		'S':'Y':'N':_	-> discard 3 >> return '\SYN'
		'E':'T':'B':_	-> discard 3 >> return '\ETB'
		'C':'A':'N':_	-> discard 3 >> return '\CAN'
		'E':'M':_	-> discard 2 >> return '\EM'
		'S':'U':'B':_	-> discard 3 >> return '\SUB'
		'E':'S':'C':_	-> discard 3 >> return '\ESC'
		'F':'S':_	-> discard 2 >> return '\FS'
		'G':'S':_	-> discard 2 >> return '\GS'
		'R':'S':_	-> discard 2 >> return '\RS'
		'U':'S':_	-> discard 2 >> return '\US'
		'S':'P':_	-> discard 2 >> return '\SP'
		'D':'E':'L':_	-> discard 3 >> return '\DEL'

-- Escaped numbers

		'o':c:_ | isOctDigit c -> do
					discard 1
					n <- lexOctal
					checkChar n
		'x':c:_ | isHexDigit c -> do
					discard 1
					n <- lexHexadecimal
					checkChar n
		c:_ | isDigit c -> do
					n <- lexDecimal
					checkChar n

		_		-> fail "Illegal escape sequence"

    where
	checkChar n | n <= 0x01FFFF = return (chr (fromInteger n))
	checkChar _		    = fail "Character constant out of range"

-- Production cntrl from section B.2

	cntrl :: Char -> Lex a Char
	cntrl c | c >= '@' && c <= '_' = return (chr (ord c - ord '@'))
	cntrl _                        = fail "Illegal control character"

-- assumes at least one octal digit
lexOctal :: Lex a Integer
lexOctal = do
	ds <- lexWhile isOctDigit
	return (parseInteger 8 ds)

-- assumes at least one hexadecimal digit
lexHexadecimal :: Lex a Integer
lexHexadecimal = do
	ds <- lexWhile isHexDigit
	return (parseInteger 16 ds)

-- assumes at least one decimal digit
lexDecimal :: Lex a Integer
lexDecimal = do
	ds <- lexWhile isDigit
	return (parseInteger 10 ds)

-- Stolen from Hugs's Prelude
parseInteger :: Integer -> String -> Integer
parseInteger radix ds =
	foldl1 (\n d -> n * radix + d) (map (toInteger . digitToInt) ds)

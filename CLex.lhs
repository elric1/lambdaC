\begin{code}

module CLex where -- (cLex, cReLex, Posn(..), TokenT(..), Token) where

import Array
import Monad

type Token = (Posn, TokenT)

--
-- A position is a filename, line number, character number and inclusion point
data Posn = Pn String Int Int (Maybe Posn)
	deriving Eq

instance Show Posn where
      showsPrec p (Pn f l c i) = showString f .
                                 showString "  at line " . shows l .
                                 showString " col " . shows c .
                                 ( case i of
                                    Nothing -> id
                                    Just p  -> showString "\n  included from " .
                                               shows p )

data TokenT =
	    -- Brackets and Braces
	      TokLbrace			-- digraph.
	    | TokRbrace			-- digraph.
	    | TokLbracket		-- digraph.
	    | TokRbracket		-- digraph.
	    | TokLparen
	    | TokRparen
	    | TokLangbrack
	    | TokRangbrack
	    -- Other punctuation
	    | TokSemi
	    | TokComma
	    | TokColon
	    | TokEllipsis

	    -- keywords (loops and conditionals)
	    | TokAuto
	    | TokBreak
	    | TokCase
	    | TokChar
	    | TokConst
	    | TokContinue
	    | TokDefault
	    | TokDo
	    | TokDouble
	    | TokElse
	    | TokEnum
	    | TokExtern
	    | TokFloat
	    | TokFor
	    | TokGoto
	    | TokIf
	    | TokInt
	    | TokLong
	    | TokRegister
	    | TokReturn
	    | TokShort
	    | TokSigned
	    | TokSizeof
	    | TokStatic
	    | TokStruct
	    | TokSwitch
	    | TokTypedef
	    | TokUnion
	    | TokUnsigned
	    | TokVoid
	    | TokVolatile
	    | TokWhile

	    -- other stuff.
	    | TokShRAssign
	    | TokShLAssign
	    | TokPlusAssign
	    | TokMinusAssign
	    | TokMultAssign
	    | TokDivAssign
	    | TokModAssign
	    | TokAndAssign
	    | TokXorAssign
	    | TokOrAssign
	    -- Still other stuff.
	    | TokShR
	    | TokShL
	    | TokInc
	    | TokDec
	    | TokPtr
	    | TokLogAnd
	    | TokLogOr
	    | TokLEq
	    | TokGEq
	    | TokEq
	    | TokDoubleEq
	    | TokNEq
	    -- more other stuff
	    | TokAssign
	    -- more other stuff
	    | TokDot
	    | TokBitAnd
	    | TokBitOr
	    | TokLogNot
	    | TokBitNot
	    | TokPlus
	    | TokMinus
	    | TokMult
	    | TokDiv
	    | TokMod
	    | TokXor
	    | TokQMark
	    -- and FINALLY
	    | TokIntConst Integer  String  -- the String is the type thing-y.
	    | TokRatConst Rational String  -- same as last comment, d00d.
	    | TokIdent String		   -- an identifier
	deriving (Show, Eq)
	-- I derive for now, maybe I'll define Show later...

{-

{L}({L}|{D})*		{ count(); return(check_type()); }

0[xX]{H}+{IS}?		{ count(); return(CONSTANT); }
0{D}+{IS}?		{ count(); return(CONSTANT); }
{D}+{IS}?		{ count(); return(CONSTANT); }
L?'(\\.|[^\\'])+'	{ count(); return(CONSTANT); }

{D}+{E}{FS}?		{ count(); return(CONSTANT); }
{D}*"."{D}+({E})?{FS}?	{ count(); return(CONSTANT); }
{D}+"."{D}*({E})?{FS}?	{ count(); return(CONSTANT); }

L?\"(\\.|[^\\"])*\"	{ count(); return(STRING_LITERAL); }

-}



formatError s p = "Syntax error in "++show p++": "++s++"\n"
cLexError   s p = error (formatError s p)

identStart	= '_':['a'..'z'] ++ ['A'..'Z']
identCont	= ['1'..'9'] ++ identStart

whitespace	= [' ', '\t', '\n']

--
-- Here, I am simply hashing on the first character read for a little
-- bit of a speedup.  rcd.  (not too confusing, either, I don't think.)

lexit p t@(x:xs) = (lexFunc ! x) p t
lexit _ _ = []

lexFunc =  array (minBound::Char, maxBound)
	(  (zip identStart (repeat getIdent))
	++ punctFuncs
	++ (zip whitespace (repeat doWhite))
	)

mkFunc  tok len p x = mkFunc' tok len p (drop len x)
mkFunc' tok len p x = (p, tok):lexit (addLen p len) x
  where addLen (Pn f l c i) y = (Pn f l (c+y) i)

star = mkFunc TokMult 1

-- just increment the position properly.
doWhite p@(Pn f l c i) (x:xs)
	| x == ' '	= lexit (Pn f l (c+1) i) xs
	| x == '\t'	= lexit (Pn f l ((c`div`8+1) * 8) i) xs
	| x == '\n'	= lexit (Pn f (l+1) 0 i) xs

getStringLit p t = []

getIdent p@(Pn f l c i) x = mkToken p tok:lexit (Pn f l (c+len) i) rest
  where	(tok,rest) = span (`elem` identCont) x
	len = length tok

mkToken x y = (x, tok)
  where (Just tok) = lookup y identTokens `mplus` Just (TokIdent y)

identTokens =
	[ ("auto", TokAuto)
	, ("break", TokBreak)
	, ("case", TokCase)
	, ("char", TokChar)
	, ("const", TokConst)
	, ("continue", TokContinue)
	, ("default", TokDefault)
	, ("do", TokDo)
	, ("double", TokDouble)
	, ("else", TokElse)
	, ("enum", TokEnum)
	, ("extern", TokExtern)
	, ("float", TokFloat)
	, ("for", TokFor)
	, ("goto", TokGoto)
	, ("if", TokIf)
	, ("int", TokInt)
	, ("long", TokLong)
	, ("register", TokRegister)
	, ("return", TokReturn)
	, ("short", TokShort)
	, ("signed", TokSigned)
	, ("sizeof", TokSizeof)
	, ("static", TokStatic)
	, ("struct", TokStruct)
	, ("switch", TokSwitch)
	, ("typedef", TokTypedef)
	, ("union", TokUnion)
	, ("unsigned", TokUnsigned)
	, ("void", TokVoid)
	, ("volatile", TokVolatile)
	, ("while", TokWhile)
	]

doDot p (_:'.':'.':xs)	= mkFunc' TokEllipsis 3 p xs
doDot p (_:xs)		= mkFunc' TokDot 1 p xs

doGT p (_:'>':'=':xs)	= mkFunc' TokShRAssign 3 p xs
doGT p (_:'>':xs)	= mkFunc' TokShR 2 p xs
doGT p (_:'=':xs)	= mkFunc' TokGEq 2 p xs
doGT p (_:'%':xs)	= mkFunc' TokRbrace 2 p xs
doGT p (_:':':xs)	= mkFunc' TokRbracket 2 p xs
doGT p (_:xs)		= mkFunc' TokRangbrack 1 p xs

doLT p (_:'<':'=':xs)	= mkFunc' TokShLAssign 3 p xs
doLT p (_:'<':xs)	= mkFunc' TokShL 2 p xs
doLT p (_:'=':xs)	= mkFunc' TokLEq 2 p xs
doLT p (_:'%':xs)	= mkFunc' TokRbrace 2 p xs
doLT p (_:':':xs)	= mkFunc' TokRbracket 2 p xs
doLT p (_:xs)		= mkFunc' TokRangbrack 1 p xs

doPlus p (_:'=':xs)	= mkFunc' TokPlusAssign 2 p xs
doPlus p (_:'+':xs)	= mkFunc' TokInc 2 p xs
doPlus p (_:xs)		= mkFunc' TokPlus 1 p xs

doMinus p (_:'=':xs)	= mkFunc' TokMinusAssign 2 p xs
doMinus p (_:'-':xs)	= mkFunc' TokDec 2 p xs
doMinus p (_:'>':xs)	= mkFunc' TokPtr 2 p xs
doMinus p (_:xs)	= mkFunc' TokMinus 1 p xs

doMult p (_:'=':xs)	= mkFunc' TokMultAssign 2 p xs
doMult p (_:xs)		= mkFunc' TokMult 1 p xs

doDiv p (_:'=':xs)	= mkFunc' TokDivAssign 2 p xs
doDiv p (_:xs)		= mkFunc' TokDiv 1 p xs

doMod p (_:'=':xs)	= mkFunc' TokModAssign 2 p xs
doMod p (_:xs)		= mkFunc' TokMod 1 p xs

doAnd p (_:'=':xs)	= mkFunc' TokAndAssign 2 p xs
doAnd p (_:'&':xs)	= mkFunc' TokLogAnd 2 p xs
doAnd p (_:xs)		= mkFunc' TokBitAnd 1 p xs

doOr p (_:'=':xs)	= mkFunc' TokOrAssign 2 p xs
doOr p (_:'&':xs)	= mkFunc' TokLogOr 2 p xs
doOr p (_:xs)		= mkFunc' TokBitOr 1 p xs

doXor p (_:'=':xs)	= mkFunc' TokXorAssign 2 p xs
doXor p (_:xs)		= mkFunc' TokXor 1 p xs

doEq p (_:'=':xs)	= mkFunc' TokDoubleEq 2 p xs
doEq p (_:xs)		= mkFunc' TokEq 1 p xs

doBang p (_:'=':xs)	= mkFunc' TokNEq 2 p xs
doBang p (_:xs)		= mkFunc' TokLogNot 1 p xs

punctFuncs =
	[ ('.', doDot)
	, (';', mkFunc TokSemi 1)
	, (',', mkFunc TokComma 1)
	, ('>', doGT)
	, ('<', doLT)
	, ('+', doPlus)
	, ('-', doMinus)
	, ('{', mkFunc TokLbrace 1)
	, ('}', mkFunc TokRbrace 1)
	, ('[', mkFunc TokLbracket 1)
	, (']', mkFunc TokRbracket 1)
	, ('(', mkFunc TokLparen 1)
	, (')', mkFunc TokRparen 1)
	, ('*', doMult)
	, ('/', doDiv)
	, ('%', doMod)
	, ('&', doAnd)
	, ('^', doXor)
	, ('|', doOr)
	, ('=', doEq)
	, ('!', doBang)
	, (':', mkFunc TokColon 1)
	, ('~', mkFunc TokBitNot 1)
	, ('?', mkFunc TokQMark 1)
	]

\end{code}

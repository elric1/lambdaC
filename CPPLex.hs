module CPPLex(PPTokenT(..), cppLex, cppLexHeader, outTokens) where

import List

--
-- A position is a filename, line number, character number and inclusion point
data Posn = Pn String Int Int (Maybe Posn)
	deriving (Eq, Show)

--
--  Okay, the syntax for pre-processing tokens is:
--
--	preprocessing-token:
--		header-name
--		identifier
--		pp-number
--		character-constant
--		string-literal
--		punctuator
--		each non-white-space character that
--                   cannot be one of the above    
-- 

data HeaderType = HTquotes | HTangbracks
  deriving (Eq, Show)

data PPTokenT =
	  PPheadername	HeaderType String
	| PPidentifier	String
	| PPpunctuator	String
	| PPnumber	String
	| PPstringlit	String
	| PPcharconst	String
	| PPotherchar	Char
	| PPnewline
	| PPspace	String
	| PPcomment	String
  deriving (Eq, Show)

cppLex t@(x:xs)	| x == '\n'		= (PPnewline, xs)
		| isSpace x		= getSpace t
		| x == '/'		= getMaybeComment t
		| isDigit x		= getNumber t
		| x == '.'		= getDot t
		| x == 'L'		= getWide t
		| x == '"'		= getStringLit xs
		| x == '\''		= getCharConst xs
		| isAlpha x		= getIdentifier t
		| otherwise		= getPunct t

cppLexHeader t@(x:xs) | x == '<'	= getHeader t
		      | x == '"'	= getHeader t
		      | otherwise	= cppLex t

getWide t@(x1:x2:xs) | x2 == '"'  = getWideStringLit xs
		     | x2 == '\'' = getWideCharConst xs
		     | otherwise  = getIdentifier t

getStringLit = get [] 
  where	get   x t@(y1:ys) | y1 == '\\' = quote x ys
	                  | y1 == '"'  = (PPstringlit x, ys)
	                  | otherwise  = get (x++[ y1 ]) ys
	quote x t@(y1:ys) | y1 == '"'  = get (x++[ y1 ]) ys
	                  | y1 == '\'' = get (x++[ y1 ]) ys
	                  | y1 == '\\' = get (x++[ y1 ]) ys
	                  | otherwise  = get (x++['\\']) t

getWideStringLit = getStringLit

getDot t@(x1:x2:xs) | isDigit x2 = getNumber t
getDot   (_:xs)	                 = (PPpunctuator ".", xs)

getMaybeComment t@(x1:x2:xs) | x2 == '/' = getLineComment t
			     | x2 == '*' = getRegularComment t
getMaybeComment t = getPunct t

getLineComment t = (PPcomment tok, rest)
  where	(tok,rest) = break (=='\n') t

getRegularComment t = (PPcomment tok, rest)
  where (tok,rest) = splitStrStr "*/" t

getNumber t = (PPnumber tok, rest)
  where	(tok,rest) = span isNumChar t
	isNumChar x = isIdentChar x || x == '.'		--XXXrcd: need e+, etc.

 -- here I cheat extensively because I `can'.  The ISO C99 spec
 -- section 6.4.7 p. 65 states that:
 --	3    If the characters ', \, ", //, or /* occur in the
 --	sequence between the < and > delimiters, the behavior is
 --	undefined. Similarly, if the characters ', \, //, or /*
 --	occur in the sequence between the " delimiters, the
 --	behavior is undefined.58) A header name preprocessing
 --	token is recognized only within a #include preprocessing
 --	directive.
 -- due to this, I just use `span' rather than processing escape
 -- characters.  I am going to fix this later to do decent escaping,
 -- but I should probably add a warning that states that this is
 -- officially undefined behaviour.

getHeader (x:xs) | x == '"' = (PPheadername HTquotes tok, rest)
		 | x == '<' = (PPheadername HTangbracks tok, rest)
  where rest = drop 1 rest'
	(tok,rest') = span (/=stopchar) xs
	stopchar    = if x == '<' then '>' else x

 --
 --XXXrcd: this one is just plain wrong...
getCharConst = get [] 
  where	get   x t@(y1:ys) | y1 == '\\' = quote x ys
	                  | y1 == '\'' = (PPcharconst x, ys)
	                  | otherwise  = get (x++[ y1 ]) ys
	quote x t@(y1:ys) | y1 == '"'  = get (x++[ y1 ]) ys
	                  | y1 == '\'' = get (x++[ y1 ]) ys
	                  | y1 == '\\' = get (x++[ y1 ]) ys
	                  | otherwise  = get (x++['\\']) t
getWideCharConst = getCharConst

getPunct t = (PPpunctuator tok, rest)
  where tok = firstmatch punctuators t
	rest = drop (length tok) t

getIdentifier x = (PPidentifier tok, rest)
  where	(tok,rest) = span isIdentChar x

getSpace x = (PPspace tok, rest)
  where (tok, rest) = span isSpace x

isIdentChar x = isAlphaNum x || x == '_'

 --
 -- punctuators' is an unordered list, for ease of programming.
punctuators' = [ "[", "]", "(", ")", "{", "}", ".", "->", "++", "--",
		"&", "*", "+", "-", "~", "!", "/", "%", "<<", ">>",
		"<", ">", "<=", ">=", "==", "!=", "^", "|", "&&", "||",
		"=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=",
		"^=", "|=", ",", "#", "##", "<:", ":>", "<%", "%>",
		"%:", "%:%:"]

 --
 -- but since order is important, we'll order it here:
punctuators = reverse $ sortBy len punctuators'
  where len a b | length a <  length b	= LT
		| length a == length b	= EQ
		| otherwise		= GT

 --
 -- splitStrStr returns a tuple of before and after the
 -- first substring is found.
splitStrStr :: String -> String -> (String, String)
splitStrStr orig = split' [] orig
  where split' x (y:ys) t@(z:zs) | y == z	= split' (x++[z]) ys zs
				 | otherwise    = split' (x++[z]) orig zs
	split' x [] z = (x,z)

--
-- firstmatch will return the first string in a list that matches the input.
firstmatch :: [String] -> String -> String
firstmatch (x:xs) y	| isPrefixOf x y = x
			| otherwise      = firstmatch xs y
firstmatch [] (y:ys)			 = [y]

 --
 -- little debuggery:

outTokens [] = ""
outTokens (PPheadername ht x:xs) = show ht++x++outTokens xs
outTokens (PPidentifier x:xs) = x++outTokens xs
outTokens (PPpunctuator x:xs) = x++outTokens xs
outTokens (PPnumber     x:xs) = x++outTokens xs
outTokens (PPstringlit  x:xs) = show x ++ outTokens xs
outTokens (PPcharconst  x:xs) = "'"++x++"'" ++ outTokens xs
outTokens (PPotherchar  x:xs) = x:outTokens xs
outTokens (PPnewline     :xs) = '\n':outTokens xs
outTokens (PPspace      x:xs) = x ++ outTokens xs
outTokens (PPcomment    _:xs) = ' ':outTokens xs

\begin{code}

module CPreProcess(preProcessFile)  where

import Array
import IO
import Monad

import CPPLex

preProcessFile s x = do	fh <- openFile x ReadMode
			c  <- hGetContents fh
			ppLineStart s c

ppLineStart s i | tok == (PPpunctuator "#") = ppDirective s rest
	        | otherwise		    = ppMacroExpand s (tok,rest)
  where (tok,rest) = cppLex i

ppDirective s i
	| tok == (PPidentifier "include") = ppInclude s rest
	| tok == (PPidentifier "define")  = ppDefine s rest
	| tok == (PPidentifier "pragma")  = ppPragma s i
	| tok == (PPidentifier "ifdef")   = ppIfdef s i
	| tok == (PPidentifier "ifndef")  = ppIfndef s i
	| tok == (PPidentifier "if")      = ppIf s i
	| otherwise			  = do x <- ppMacroExpand s (tok,rest)
					       return (PPpunctuator "#":x)
  where (tok,rest) = cppLex i

ppMacroExpand s (tok,[])   	= return (tok:[])
ppMacroExpand s (tok,rest)
	| tok == PPnewline 	= do x <- ppLineStart s rest
				     return (PPnewline:x)
	| isFMacro tok		= ppExpandFIdentifier s (tok,rest)
	| isMacro tok		= ppExpandIdentifier s (tok,rest)
	| otherwise		= do y <- ppMacroExpand s (tok',rest')
				     return (tok:y)
  where (tok',rest') = cppLex rest
	isMacro (PPidentifier t) = getMacro s (NoArg t) /= Nothing
	isMacro _		 = False
	isFMacro _		 = False

ppExpandIdentifier s (PPidentifier tok,rest)
				= do y <- ppMacroExpand s (tok',rest')
				     return (toks++y)
  where (tok',rest') = cppLex rest
	(Just macEx) = getMacro s (NoArg tok)
	toks = macroReplace macEx [] -- (getParen (tok',rest'))

{-
ppExpandFIdentifier s t@(PPidentifier tok, rest)
	| length parenExpr == 0		= ppExpandIdentifer s t
	| otherwise			= do y <- ppMacroExpand s (tok'',rest'')
					     return (toks++y)
  where	(tok',rest') = cppLex rest
	parenExpr = getParenExpr s (tok',rest')
-}

ppExpandFIdentifier = f 0 []
  where f _ _ = ppExpandIdentifier

{-	XXX XAX elric: this is borked!
ppExpandFIdentifier = f 0 []
  where	f 0 s (PPpunctuator ")", rest) = do y <- ppMacroExpand s (tok',rest')
					    return (toks++y)
	(tok',rest') = cppLex rest
	(Just macEx) = getMacro s (NoArg tok)
	toks = macroReplace macEx []
-}

ppInclude s i = do x <- preProcessFile s incFile
		   y <- ppLineStart s rest
		   return (x ++ y)
  where (PPheadername _ incFile,rest) = ppIncludeParse i

ppIncludeParse i | (length inc) == 1	= head inc
		 | otherwise		= error "failed to parse include"
  where	tmp = takeWhile ((/=PPnewline).fst) (thread cppLexHeader i)
	inc = getHName tmp
	getHName = filter (isPPheadername . fst)

isPPheadername (PPheadername _ _) = True
isPPheadername _ = False

ppPragma s i = ppMacroExpand s (PPpunctuator "#", i)
ppIfdef s i  = ppMacroExpand s (PPpunctuator "#", i)
ppIfndef s i = ppMacroExpand s (PPpunctuator "#", i)
ppIf s i     = ppMacroExpand s (PPpunctuator "#", i)

ppDefine s i = ppLineStart news rest
  where (news,rest) = pMacro s i

\end{code}

Okay, let's do the silly replacement code.  This will obviously
have to be rewritten...  We will store the macros as a list of
key-value pairs.  So, here we index in a `Macro' to a `MacroExpansion'.
The Macro represents the only unique macro definitions.

\begin{code}

pMacro s i = (defnM toks:s, rest)
  where	tmp = takeWhile ((/=PPnewline).fst) (thread cppLex i)
	toks = map fst tmp
	rest = snd $ last tmp

defnM i = (NoArg ident, ME 0 (map Right rlist))
  where	tmp    = dropWhile ppIsSpace i
	ident' = head tmp
	rlist  = dropWhile ppIsSpace (tail tmp)
	(PPidentifier ident) = ident'

ppIsIdentifier (PPidentifier _) = True
ppIsIdentifier _ = False

ppIsSpace (PPspace _) = True
ppIsSpace _ = False

data Macro = NoArg String
	   | Args  String
  deriving (Eq, Show)

data MacroExpansion = ME Int [Either Int PPTokenT]
  deriving (Eq, Show)

getMacro s macro = lookup macro s
putMacro s macro expansion = (macro,expansion):s

macroReplace (ME x toks) args = do tok <- toks
				   return (replace tok args)

replace (Left x) args = (args++repeat (PPspace " ")) !! x
replace (Right x) _   = x

\end{code}
what we want is:

	(toks,rest) = cppLex s
	(toks',rest') = cppLex rest
	(toks'',rest'') = cppLex rest'
	.
	.
	.

so, let's see...
\begin{code}

thread :: (s->(a,s)) -> s -> [(a,s)]
thread f s = (one,two):thread f two
  where (one,two) = f s

\end{code}

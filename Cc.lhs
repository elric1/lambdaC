#!/usr/pkg/bin/runhugs -Fcat
\begin{code}
module Main where

import CPPLex
import CPreProcess
import Cprogram(Cprogram,parseCFile,munge)
import AlphaASM(alphaAsmOut)
import I386ASM(i386asmOut)
import System
import IO
import List

main = do	x   <- getArgs
		doIt $ parseOpts x

doIt :: ([Opts], [(FType, String)]) -> IO ()
doIt (x,file:args) = do processFile x file
			doIt (x,args)
doIt _			 = return ()

--
-- So, the problem that we have here is that we need to set up a
--	pipeline of preprocess -> compiler -> assembler -> linker
-- for now, we are going to take the easy way out and assume that
-- each of these will be separate processes.  Once we have written
-- an assembler and a linker in Haskell, we shall do it all here.

--
-- processFile takes a single file and compiles it to the level which
-- is expected and then returns the filename for future reference.

processFile :: [Opts] -> (FType, String) -> IO String
processFile x y@(yt,yn)
	| EFlag `elem` x	= do preProcessIt x y
				     return []
	| SFlag `elem` x	= do ret <- compileC x yn (Just $ makeSuffixS yn)
				     return ret
	| otherwise		= do ret <- compileC x yn Nothing
				     return ret
  where	makeSuffixS x = (take (length x - 2) x) ++ ".s"


preProcessIt :: [Opts] -> (FType, String) -> IO ()
preProcessIt x (FTc, file) = do y <- preProcessFile [] file
				putStr $ outTokens y
preProcessIt x (FTS, file) = do y <- preProcessFile [] file
				putStr $ outTokens y

--
-- compileC should take C -> whatever is desired.  Right now, that
-- is only assembly.
--
-- If not passed an outfile, this function will create one in "/tmp"
--
compileC :: [Opts] -> String -> Maybe String -> IO String
compileC x inFile outFile = do	(fh,fn) <- open outFile
				proggie <- parseCFile inFile
				hPutStr fh (asmOut x $ munge $ proggie)
				return fn
  where	open (Just fn) = do fh <- openFile fn WriteMode
			    return (fh,fn)
	open Nothing = mkstemp "/tmp/ccXXXXXX.s"

asmOut :: [Opts] -> (Cprogram -> String)
asmOut x	| AlphaArch `elem` x	= alphaAsmOut
		| I386Arch  `elem` x	= i386asmOut
		| otherwise		= alphaAsmOut

--
-- XXXrcd: this should actually construct a temp file!
mkstemp :: String -> IO (Handle, String)
mkstemp x = do fh <- openFile x WriteMode
	       return (fh, x)

--
-- For FType we use the convention: FTxx where xx is the extension.
-- (since we're acting like cc.)
-- We aren't bothering with C++ source, yet.
data FType = FTa | FTc | FTi | FTo | FTs | FTS
  deriving Eq

data Opts = CFlag | EFlag | SFlag | AlphaArch | I386Arch | IncludeDir String
  deriving Eq

parseOpts :: [String] -> ([Opts], [(FType, String)])
parseOpts = p [] []
  where	p xs ys ("-S":zs) = p (SFlag:xs) ys zs
	p xs ys ("-E":zs) = p (EFlag:xs) ys zs
	p xs ys ("-c":zs) = p (CFlag:xs) ys zs
	p xs ys (z:zs)
		| "-I" `isPrefixOf` z	= p ((IncludeDir (drop 2 z)):xs) ys zs
	p xs ys (z:zs)
		| z == "-malpha"	= p (AlphaArch:xs) ys zs
		| z == "-mi386"		= p (I386Arch:xs) ys zs
	p xs ys (z:zs)
		| ".a" `isSuffixOf` z	= p xs ((FTa,z):ys) zs
		| ".c" `isSuffixOf` z	= p xs ((FTc,z):ys) zs
		| ".i" `isSuffixOf` z	= p xs ((FTi,z):ys) zs
		| ".o" `isSuffixOf` z	= p xs ((FTo,z):ys) zs
		| ".s" `isSuffixOf` z	= p xs ((FTs,z):ys) zs
		| ".S" `isSuffixOf` z	= p xs ((FTS,z):ys) zs
	p xs ys zs			= (xs, ys)

\end{code}

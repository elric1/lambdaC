#!/usr/pkg/bin/runhugs -Fcat
\begin{code}
module Main where

import Cprogram(parseC,munge)
import AlphaASM(alphaAsmOut)
import I386ASM(i386asmOut)
import System
import IO

main = do	x <- getArgs
		y <- openFile (head x) ReadMode
		z <- hGetContents y
		putStr $ alphaAsmOut $ munge $ parseC z
\end{code}

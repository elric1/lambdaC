#!/usr/pkg/bin/runhugs -Fcat
\begin{code}
module Main where

import Cprogram
import System
import IO

main = do	x <- getArgs
		y <- openFile (head x) ReadMode
		z <- hGetContents y
		putStrLn $ show $ parseC z
\end{code}

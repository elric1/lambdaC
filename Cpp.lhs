#!/usr/pkg/bin/runhugs -Fcat
\begin{code}
module Main where

import CPPLex
import CPreProcess
import System
import IO

main = do	x <- getArgs
		y <- preProcessFile [] (head x)
		putStr $ outTokens y
\end{code}

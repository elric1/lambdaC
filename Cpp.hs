#!/usr/pkg/bin/runhugs -Fcat

module Main where

import CPPLex
import CPreProcess
import System
import IO

main = do	x <- getArgs
		y <- preProcessFile [] (head x)
		putStr $ outTokens y

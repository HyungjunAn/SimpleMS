{-# LANGUAGE TemplateHaskell #-}

module Main where
import SimpleMS

import System.Environment (getArgs)

import Sudoku
import Control.Exception
import Data.Maybe

afterFunc xs = print (length $ filter (=="1") xs)

check :: String -> String
check d = if isJust (solve d)
            then "1"
            else "0"
            
mkSlaveJob "unordered" 'check

main = do 
  args <- getArgs
  case args of
    ["master", host, port, f] -> do
        input' <- readFile f
        let input = lines input'
        $(runMS "master" "%H%P") host port input afterFunc
    ["slave",  host, port] -> do
        $(runMS "slave"  "%H%P") host port

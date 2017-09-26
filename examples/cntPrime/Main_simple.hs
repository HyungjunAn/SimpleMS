{-# LANGUAGE TemplateHaskell #-}

module Main where
import SimpleMS

import System.Environment (getArgs)

afterFunc :: [Int] -> IO ()
afterFunc xs = print (sum xs)

isPrime :: Int -> Bool
isPrime n = if isPrime' n 1 0 == 1
              then True
              else False
            where isPrime' :: Int -> Int -> Int -> Int
                  isPrime' n i cnt = if n == i 
                              then cnt
                              else if n `rem` i == 0
                                  then isPrime' n (i+1) (cnt+1)
                                  else isPrime' n (i+1) cnt
check :: Int -> Int
check d = if isPrime d
            then 1
            else 0

mkSlaveJob "unordered" 'check

main = do 
  args <- getArgs
  case args of
    ["master", host, port, f] -> do
        input'' <- readFile f
        let input' = lines input''
            input  = map read input' :: [Int]
        $(runMS "master" "%H%P") host port input afterFunc
    ["slave",  host, port] -> do
        $(runMS "slave"  "%H%P") host port

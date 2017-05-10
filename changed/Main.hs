{-# LANGUAGE TemplateHaskell #-}
module Main where
import System.Environment (getArgs)

import Lib

incr :: Int -> Int
incr x = x + 1

sqr :: Int -> Int
sqr x = x ^ 2

input = [1, 2, 3, 4] :: [Int]

prop :: Int -> [Int] -> Bool
prop n xs | sum xs > n  = True
          | otherwise   = False

afterFunc :: [Int] -> IO()
{-afterFunc xs = putStrLn $ show $ sum xs-}
afterFunc xs = putStrLn $ show xs
--------------------------------------------------------------------------
--mkSlaveJob 'sqr
mkSlaveJob 'incr

main = do
  args <- getArgs
  case args of
    ["master", host, port] ->
      {-$(simpleMS "master" "%H%P%R") host port "unordered" input afterFunc -}
      {-$(simpleMS "master" "%H%P%R") host port "ordered" input afterFunc -}
      $(simpleMS "master" "%H%P") host port input afterFunc 
      {-$(simpleMS "master" "%H%P%L%R") host port (prop 200) "unordered" input afterFunc -}
      {-$(simpleMS "master" "%H%P%R") host port "unordered" rtable input afterFunc -}
    ["slave",  host, port] -> 
      $(simpleMS "slave" "%H%P") host port 

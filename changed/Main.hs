{-# LANGUAGE TemplateHaskell, KindSignatures #-}
module Main where
import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet

import Lib

plus :: Int -> Int
plus x = x + 1

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
slaveJob :: ProcessId -> Process()
{-slaveJob = $(mkSlaveJob) plus-}
slaveJob = $(mkSlaveJob) sqr
remotable ['slaveJob]
clos = $(mkClosure 'slaveJob)
rtable = __remoteTable initRemoteTable

main = do
  args <- getArgs
  case args of
    ["master", host, port] ->
      {-$(simpleMS "master" "%H%P%R%r") host port (prop 100) rtable input clos afterFunc -}
      $(simpleMS "master" "%H%P%R%r") host port (prop 200) "unordered" rtable input clos afterFunc 
      {-$(simpleMS "master" "%H%P%r") host port "unordered" rtable input clos afterFunc -}
    ["slave",  host, port] -> 
      $(simpleMS "slave" "%H%P") host port rtable 

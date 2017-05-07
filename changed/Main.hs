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
prop n xs | sum xs < n  = True
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
recProc = $(mkRecProc "unseq")

main = do
  args <- getArgs
  case args of
    ["master", host, port] -> $(simpleMS "master") host port clos input recProc rtable afterFunc (prop 100)
    ["slave",  host, port] -> $(simpleMS "slave" ) host port rtable

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

input = [1, 2, 3, 4] :: [Int]

afterFunc :: [Int] -> IO()
afterFunc xs = putStrLn $ show $ sum xs
--------------------------------------------------------------------------
slaveJob :: ProcessId -> Process()
slaveJob = $(mkSlaveJob) plus
remotable ['slaveJob]
clos = $(mkClosure 'slaveJob)
rtable = __remoteTable initRemoteTable
recProc = $(mkRecProc "unseq")

main = do
  args <- getArgs
  case args of
    ["master", host, port] -> $(simpleMS "master") host port clos input recProc rtable afterFunc
    ["slave",  host, port] -> $(simpleMS "slave" ) host port rtable

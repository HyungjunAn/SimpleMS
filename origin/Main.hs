{-# LANGUAGE TemplateHaskell, KindSignatures #-}
module Main where
import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forever, forM, forM_)

plus :: Int -> Int
plus x = x + 1

input = [1, 2, 3, 4] :: [Int]

afterFunc :: [Int] -> IO()
afterFunc xs = putStrLn $ show $ sum xs
--------------------------------------------------------------------------
slaveJob :: ProcessId -> Process()
slaveJob = \them -> do
                      forever $ do
                            n <- expect
                            send them (plus n)
remotable ['slaveJob]
clos = $(mkClosure 'slaveJob)
rtable = __remoteTable initRemoteTable
recProc len = loop len []
  where
    loop 0 xs = return xs
    loop n xs = do
          x <- expect
          loop (n-1) (x:xs)

masterJob backend slaves = do
          us <- getSelfPid
          slaveProcesses <- forM slaves $ 
            \nid -> spawn nid (clos us)
          spawnLocal $ forM_ (zip input (cycle slaveProcesses)) $
            \(m, them) -> send them m
          res <- recProc (length input)
          liftIO (afterFunc res)

main = do
  args <- getArgs
  case args of
    ["master", host, port] -> do
            backend <- initializeBackend host port rtable
            startMaster backend (masterJob backend)
    ["slave",  host, port] -> do
            backend <- initializeBackend host port rtable
            startSlave backend
{-# LANGUAGE TemplateHaskell, KindSignatures #-}
module Main where
import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Data.Binary
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forever, forM, forM_)
import Control.Distributed.Process.Serializable

import Lib

plus :: Int -> Int
plus x = x + 1



ss = [1, 2, 3, 4] :: [Int]
good :: ProcessId -> Process ()
good = $(mkgood "plus")

remotable ['good]

mailBox :: Int -> Process [Int]
mailBox = go []
        where
          go :: [Int] -> Int -> Process [Int]
          go xs 0 = return xs
          go xs n = do
                        num <- expect 
                        go (num:xs) (n-1)

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  us <- getSelfPid
  slaveProcesses <- forM slaves $
          \nid -> spawn nid ($(mkClosure 'good) us)
  spawnLocal $ forM_ (zip ss (cycle slaveProcesses)) $
          \ (m, them) -> send them m
  res <- mailBox (length ss)
  liftIO . putStrLn $ show res

  -- Do something interesting with the slaves
  --liftIO . putStrLn $ "Slaves: " ++ show slaves
  -- Terminate the slaves when the master terminates (this is optional)
  --terminateAllSlaves backend

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
      {-backend <- initializeBackend host port rtable-}
      backend <- initializeBackend host port (__remoteTable initRemoteTable)
      startMaster backend (master backend)
    ["slave", host, port] -> do
      putStrLn "Test"
      {-backend <- initializeBackend host port rtable-}
      backend <- initializeBackend host port (__remoteTable initRemoteTable)
      startSlave backend

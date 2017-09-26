{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Serializable
import Control.Monad

import Sudoku
import Control.Exception
import Data.Maybe

afterFunc :: [String] -> IO ()
afterFunc xs = print (length $ filter (=="1") xs)

slaveJob :: ProcessId -> Process()
slaveJob = \them -> do
                      forever $ do
                            d <- expect
                            send them (check d)

check :: String -> String
check d = if isJust (solve d)
            then "1"
            else "0"

remotable ['slaveJob]
clos = $(mkClosure 'slaveJob)
rtable = __remoteTable initRemoteTable

main = do 
  args <- getArgs
  case args of
    ["master", host, port, f] -> do
        input' <- readFile f
        let input = lines input'
        backend <- initializeBackend host port rtable
        startMaster backend (masterJob input)
    ["slave",  host, port] -> do
        backend <- initializeBackend host port rtable
        startSlave backend
      
masterJob input slaves = do
          us <- getSelfPid
          slaveProcesses <- forM slaves $ 
            \nid -> spawn nid (clos us)
          spawnLocal $ forM_ (zip input (cycle slaveProcesses)) $
            \(m, them) -> send them m
          res <- receive
          liftIO (afterFunc res)
          where
            receive = loop (length input) []
                  where
                    loop 0 xs = return xs
                    loop n xs = do
                    x <- expect
                    loop (n-1) (xs ++ [x])

{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Serializable
import Control.Monad

afterFunc :: [Int] -> IO ()
afterFunc xs = print (sum xs)

slaveJob :: ProcessId -> Process()
slaveJob = \them -> do
                      forever $ do
                            d <- expect
                            send them (check d)
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

remotable ['slaveJob]
clos = $(mkClosure 'slaveJob)
rtable = __remoteTable initRemoteTable

main = do 
  args <- getArgs
  case args of
    ["master", host, port, f] -> do
        input'' <- readFile f
        let input' = lines input''
            input  = map read input' :: [Int]
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

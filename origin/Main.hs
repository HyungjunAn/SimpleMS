{-# LANGUAGE TemplateHaskell #-}
module Main where
import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forever, forM, forM_)

input :: [Int]
input = [1 .. 10000] :: [Int]

incr :: Int -> Int
incr x = x + 1

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2) 

afterFunc :: [Int] -> IO ()
afterFunc xs = putStrLn $ show $ sum xs
{-afterFunc xs = putStrLn $ show xs-}
--------------------------------------------------------------------------
slaveJob :: ProcessId -> Process()
slaveJob = \them -> do
                      forever $ do
                            n <- expect
                            send them (incr n)

remotable ['slaveJob]
clos = $(mkClosure 'slaveJob)
rtable = __remoteTable initRemoteTable

masterJob input slaves = do
          us <- getSelfPid
          slaveProcesses <- forM slaves $ 
            \nid -> spawn nid (clos us)
          spawnLocal $ forM_ (zip input (cycle slaveProcesses)) $
            \(m, them) -> send them m
          res <- loop (length input) []
          liftIO (afterFunc res)
          where
            loop 0 xs = return xs
            loop n xs = do
                x <- expect
                loop (n-1) (xs ++ [x])

main = do
  args <- getArgs
  case args of
    ["master", host, port] -> do
            backend <- initializeBackend host port rtable
            startMaster backend (masterJob input)
    ["slave",  host, port] -> do
            backend <- initializeBackend host port rtable
            startSlave backend

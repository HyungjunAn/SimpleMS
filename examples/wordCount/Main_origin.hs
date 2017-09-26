{-# LANGUAGE TemplateHaskell #-}

module Main where
import Data.List.Split

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Serializable
import Control.Monad

merge n xs = merge' 0 [] xs []
    where
      merge' i tmps []     res = res ++ [tmps]
      merge' i tmps (x:xs) res | i == n     = merge' 0 [] (x:xs) (res ++ [tmps])
                               | otherwise  = merge' (i+1) (tmps ++ x) xs res

while xs = case length xs == 1 of
                  True  -> xs !! 0
                  False -> while $ map count (merge 2 xs)

findMax xs = findMax' xs ("a", 0)
      where
        findMax' [] res = res
        findMax' ((x, n):xs) (m, old_n) | n > old_n = findMax' xs (x, n)
                                        | n == old_n && x < m  = findMax' xs (x, old_n)
                                        | otherwise = findMax' xs (m, old_n)

count :: [(String, Int)] -> [(String, Int)]
count xs = count' xs []
  where 
    count' [] res = res
    count' (x:xs) res = count' xs' $ res ++ [x']
        where 
          (x', xs') = foldl f (x, []) xs
            where 
              f (a, xs) x =  case x1 == x2 of
                                  True  -> ((x1, n1 + n2), xs)
                                  False -> (a, xs ++ [x])
                                  where
                                    (x1, n1) = a
                                    (x2, n2) = x

afterFunc :: [(String, Int)] -> IO ()
afterFunc xs = putStrLn $ show $ findMax $ xs

prop xs = length xs == 1
------------------------------------------------
slaveJob :: ProcessId -> Process()
slaveJob = \them -> do
                      forever $ do
                            strs <- expect
                            send them (count strs)

remotable ['slaveJob]
clos = $(mkClosure 'slaveJob)
rtable = __remoteTable initRemoteTable
main = do 
  args <- getArgs
  case args of
    ["master", host, port, f] -> do
        content <- readFile f
        let input''  = lines content
            input'   = zip input'' ([1, 1..] :: [Int])
            input    = chunksOf 10 input'
        --putStrLn $ show $ findMax $ count input
        backend <- initializeBackend host port rtable
        startMaster backend (masterJob input)
    ["slave",  host, port] -> do
        backend <- initializeBackend host port rtable
        startSlave backend
      
masterJob :: [[(String, Int)]] -> [NodeId] -> Process ()
masterJob input slaves = do
          us <- getSelfPid
          slaveProcesses <- forM slaves $ 
            \nid -> spawn nid (clos us)
          res <- loop slaveProcesses input
          liftIO (afterFunc res)
          where
            loop :: [ProcessId] -> [[(String, Int)]] -> Process [(String, Int)]
            loop slvPrcsss input = do
                        spawnLocal $ forM_ (zip input (cycle slvPrcsss)) $
                          \(m, them) -> send them m
                        res <- receive
                        if prop res
                          then return (res !! 0)
                          --TODO: if length of res is less then n do not merge
                          else loop slvPrcsss (merge 3 res)
                        where
                            receive = loop (length input) []
                                where
                                    loop 0 xs = return xs
                                    loop n xs = do
                                        x <- expect
                                        loop (n-1) (xs ++ [x])

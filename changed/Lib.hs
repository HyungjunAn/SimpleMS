{-# LANGUAGE TemplateHaskell #-}
module Lib where

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forever, forM, forM_)

mkSlaveJob = [| \userFunc -> \them -> do
                        forever $ do
                            n <- expect
                            send them (userFunc n) |]

simpleMS "master" = [| \host -> \port -> \clos -> \input -> \recProc -> \rtable -> \afterFunc -> do 
    backend <- initializeBackend host port rtable
    startMaster backend $
      (\backend -> \slaves -> do
          us <- getSelfPid
          slaveProcesses <- forM slaves $
            \nid -> spawn nid (clos us)
          spawnLocal $ forM_ (zip input (cycle slaveProcesses)) $
            \(m, them) -> send them m
          res <- recProc (length input)
          liftIO (afterFunc res)) backend |]

simpleMS "slave" = [| \host -> \port -> \rtable -> do 
    backend <- initializeBackend host port rtable
    startSlave backend |]

mkRecProc "unseq" = [|  let loop 0 xs = return xs
                            loop n xs = do
                              x <- expect
                              loop (n-1) (x:xs)
                        in \len -> loop len [] |]

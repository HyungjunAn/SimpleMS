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

simpleMS "master" = [| 
          let loop prop input slaveProcesses recProc  = do 
                        res <- masterJob input slaveProcesses recProc
                        if prop res
                          then return res
                          else loop prop res slaveProcesses recProc
          in 
            \host -> \port -> \clos -> \input -> \recProc -> \rtable -> \afterFunc -> \prop -> do 
              backend <- initializeBackend host port rtable
              startMaster backend $
                \slaves -> do
                us <- getSelfPid
                slaveProcesses <- forM slaves $
                  \nid -> spawn nid (clos us)
                res <- loop prop input slaveProcesses recProc
                liftIO (afterFunc res) |]
simpleMS "slave" = [| \host -> \port -> \rtable -> do 
    backend <- initializeBackend host port rtable
    startSlave backend |]

masterJob input slaveProcesses recProc = do
          spawnLocal $ forM_ (zip input (cycle slaveProcesses)) $
            \(m, them) -> send them m
          res <- recProc (length input)
          return res

mkRecProc "unseq" = [|  let loop 0 xs = return xs
                            loop n xs = do
                              x <- expect
                              loop (n-1) (x:xs)
                        in \len -> loop len [] |]

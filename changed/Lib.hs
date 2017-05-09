{-# LANGUAGE TemplateHaskell #-}
module Lib where

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forever, forM, forM_)
import Control.Distributed.Process.Serializable

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

ffff funcName = remotableDecl [
  [d| slaveJob :: ProcessId -> Process();
      slaveJob them = do
                        forever $ do
                            n <- expect
                            send them ($(varE funcName) n) |] ]

exexex = [d| clos = $(mkClosure (mkName "slaveJob"))
             rtable = $(varE (mkName "__remoteTableDecl")) initRemoteTable |]

data Format =  H | P | R | L

simpleMS :: String -> String -> Q Exp
simpleMS ms s = gen ms (parse s) [|([], \_ -> True)|]
    where
      parse :: String -> [Format]
      parse [] = []
      parse ('%':'H':xs) = H : parse xs
      parse ('%':'P':xs) = P : parse xs
      parse ('%':'r':xs) = R : parse xs  --receive order parse ('%':'L':xs) = L : parse xs  --recursion 
      gen :: String -> [Format] -> Q Exp -> Q Exp
      gen "master" [] x = [|  let ss = fst $x
                                  h = check ss "H_"
                                  p = check ss "P_"
                                  r = check ss "R_"
                                  host | h /= "" = h
                                       | otherwise = "127.0.0.1"
                                  port | p /= "" = p
                                       | otherwise = "100"
                                  recOrd | r /= ""   = r
                                         | otherwise = "unordered"
                                  prop = snd $x
                              in 
                                runMaster host port prop (mkRecProc recOrd) $(varE (mkName "rtable")) $(varE (mkName "clos"))|]
      gen "slave" [] x = [| let ss = fst $x
                                h = check ss "H_"
                                p = check ss "P_"
                                host | h /= "" = h
                                     | otherwise = "127.0.0.1"
                                port | p /= "" = p
                                     | otherwise = "101"
                            in runSlave host port $(varE (mkName "rtable")) |]
      gen ms (H : xs) x = [| \host -> $(gen ms xs [| (("H_" ++ host):(fst $x), snd $x) |]) |]
      gen ms (P : xs) x = [| \port -> $(gen ms xs [| (("P_" ++ port):(fst $x), snd $x) |]) |]
      gen ms (L : xs) x = [| \prop -> $(gen ms xs [|  (fst $x, prop) |]) |]
      gen ms (R : xs) x = [| \rcvO -> $(gen ms xs [| (("R_" ++ rcvO):(fst $x), snd $x) |]) |]

check :: [String] -> String -> String
check [] _ = ""
check (s:ss) str | take (length str) s == str = drop (length str) s
                 | otherwise                  = check ss str

runMaster host port prop recProc rtable clos input afterFunc = do 
          backend <- initializeBackend host port rtable
          startMaster backend $
            \slaves -> do
              us <- getSelfPid
              slaveProcesses <- forM slaves $
                \nid -> spawn nid (clos us)
              res <- loop prop input slaveProcesses recProc
              liftIO (afterFunc res)
              where
                loop prop input slaveProcesses recProc  = do 
                    res <- masterJob input slaveProcesses recProc
                    if prop res
                      then return res
                      else loop prop res slaveProcesses recProc

runSlave host port rtable = do 
                      backend <- initializeBackend host port rtable
                      startSlave backend

masterJob input slaveProcesses recProc = do
          spawnLocal $ forM_ (zip input (cycle slaveProcesses)) $
            \(m, them) -> send them m
          res <- recProc (length input)
          return res

mkRecProc "unordered" len = loop len []
                        where
                          loop 0 xs = return xs
                          loop n xs = do
                              x <- expect
                              loop (n-1) (x:xs)

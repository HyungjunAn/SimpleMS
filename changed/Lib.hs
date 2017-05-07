{-# LANGUAGE TemplateHaskell #-}
module Lib where

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forever, forM, forM_)

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

mkSlaveJob = [| \userFunc -> \them -> do
                        forever $ do
                            n <- expect
                            send them (userFunc n) |]


data Format = R | H | P | RCV

simpleMS :: String -> String -> Q Exp
simpleMS ms s = gen ms (parse s) [|([], \_ -> True)|]
    where
      parse :: String -> [Format]
      parse [] = []
      parse ('%':'H':xs) = H   : parse xs
      parse ('%':'P':xs) = P   : parse xs
      parse ('%':'r':xs) = RCV : parse xs  --receive order
      parse ('%':'R':xs) = R   : parse xs  --recursion

      {-gen []         (ms, h, p, f, rp) = [| let host | h /= "" = h
                                                     | otherwise = "127.0.0.1"
                                                port | p /= "" = p
                                                     | otherwise = "80"
                                                recProc | rp /= ""  = mkRecProc "unordered"
                                                        | otherwise    = mkRecProc "unordered"
                                                prop _ = True
                                            in
                                              if (ms == "m")
                                                then runMaster host port prop recProc
                                                else runSlave  host port |]-}
                                          
      {-gen (M   : xs) (ms, h, p, f, rp) = gen xs ("m", h, p, f, rp)
      gen (S   : xs) (ms, h, p, f, rp) = gen xs ("s", h, p, f, rp)
      gen (H   : xs) (ms, h, p, f, rp) = [| \host    -> $(gen xs (ms,  host, p,    f, rp     )) |]
      gen (P   : xs) (ms, h, p, f, rp) = [| \port    -> $(gen xs (ms,  h,    port, f, rp     )) |]
      gen (RCV : xs) (ms, h, p, f, rp) = [| \recProc -> $(gen xs (ms,  h,    p     f, recProc)) |]-}

      gen :: String -> [Format] -> Q Exp -> Q Exp
      gen "master" [] x = [|  let ss = fst $x
                                  h = check ss "H_"
                                  p = check ss "P_"
                                  rp = check ss "REC_"
                                  host | h /= "" = h
                                       | otherwise = "127.0.0.1"
                                  port | p /= "" = p
                                       | otherwise = "100"
                                  recProc | rp /= ""  = rp
                                          | otherwise = "unordered"
                                  prop = snd $x
                              in runMaster host port prop (mkRecProc recProc) |]
      gen "slave" [] x = [| let ss = fst $x
                                h = check ss "H_"
                                p = check ss "P_"
                                host | h /= "" = h
                                     | otherwise = "127.0.0.1"
                                port | p /= "" = p
                                     | otherwise = "101"
                            in runSlave host port |]
      gen ms (H   : xs) x = [| \host -> $(gen ms xs [| (("H_" ++ host):(fst $x), snd $x) |]) |]
      gen ms (P   : xs) x = [| \port -> $(gen ms xs [| (("P_" ++ port):(fst $x), snd $x) |]) |]
      gen ms (R   : xs) x = [| \prop -> $(gen ms xs [|  (fst $x, prop) |]) |]
      gen ms (RCV : xs) x = [| \rcvO -> $(gen ms xs [| (("RCV_" ++ rcvO):(fst $x), snd $x) |]) |]

check :: [String] -> String -> String
check [] _ = ""
check (s:ss) str | take (length str) s == str = drop (length str) s
                 | otherwise                  = check ss str

runMaster host port prop recProc rtable input clos afterFunc = do 
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

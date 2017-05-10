{-# LANGUAGE TemplateHaskell #-}
module Lib where

-- import System.Environment (getArgs)
import System.Process(runInteractiveCommand, waitForProcess)
--import Filesystem.Path.CurrentOS
import System.Exit (ExitCode)
import System.IO (hGetContents)

import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forever, forM, forM_)
import Control.Distributed.Process.Serializable

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

mkSlaveJob funcName = remotableDecl [
  [d| slaveJob :: ProcessId -> Process();
      slaveJob them = do
                        forever $ do
                            n <- expect
                            send them ($(varE funcName) n) |] ]

data Format =  H | P | R | L

simpleMS :: String -> String -> Q Exp
simpleMS ms s = gen ms (parse s) [|([], \_ -> True)|]
    where
      parse :: String -> [Format]
      parse [] = []
      parse ('%':'H':xs) = H : parse xs
      parse ('%':'P':xs) = P : parse xs
      parse ('%':'r':xs) = R : parse xs  --receive order 
      parse ('%':'L':xs) = L : parse xs  --recursion 

      gen :: String -> [Format] -> Q Exp -> Q Exp
      gen "master" [] x = [|  let ss = fst $x
                                  [h, p, r] = map (findOptStr ss) ["H_", "P_", "R_"]
                                  host | h /= "" = h
                                       | otherwise = "127.0.0.1"
                                  port' | p /= "" = p
                                        | otherwise = "unknown"
                                  recOrd | r /= ""   = r
                                         | otherwise = "unordered"
                                  prop = snd $x
                                  clos = $(mkClosure (mkName "slaveJob"))
                                  rtable = $(varE (mkName "__remoteTableDecl")) initRemoteTable 
                              in 
                                \input -> \afterFunc -> do
                                  portt <- getUnusedPort port'
                                  let port = fromJust portt
                                  runMaster host port prop (mkRecProc recOrd) rtable clos input afterFunc |] 
      gen "slave" [] x = [| let ss = fst $x
                                [h, p] = map (findOptStr ss) ["H_", "P_"]
                                host | h /= "" = h
                                     | otherwise = "127.0.0.1"
                                port' | p /= "" = p
                                      | otherwise = "unknown"
                                rtable = $(varE (mkName "__remoteTableDecl")) initRemoteTable 
                            in 
                              do
                                portt <- getUnusedPort port'
                                let port = fromJust portt
                                runSlave host port rtable |]
      gen ms (H : xs) x = [| \host -> $(gen ms xs [| (("H_" ++ host):(fst $x), snd $x) |]) |]
      gen ms (P : xs) x = [| \port -> $(gen ms xs [| (("P_" ++ port):(fst $x), snd $x) |]) |]
      gen ms (L : xs) x = [| \prop -> $(gen ms xs [|                ((fst $x), prop  ) |]) |]
      gen ms (R : xs) x = [| \rcvO -> $(gen ms xs [| (("R_" ++ rcvO):(fst $x), snd $x) |]) |]
      
findOptStr :: [String] -> String -> String
findOptStr [] _ = ""
findOptStr (s:ss) str | take (length str) s == str = drop (length str) s
                      | otherwise                  = findOptStr ss str

getHostIpAndUnusedPort :: String -> String -> IO (String, String)
getHostIpAndUnusedPort host port = do
                            let host' | host == "" = "127.0.0.1"
                                      | otherwise  = host
                            port' <- getUnusedPort port
                            return (host', fromJust port')

getUnusedPort :: String -> IO (Maybe String)
getUnusedPort s = do
    used <- getProcessOutput $
                    "lsof -i -nP | grep LISTEN | awk '{print $(NF-1)}' | sort -u | awk -F ':' '{print $NF}'"
    let usedPorts = filter (\s -> isInteger s) $ splitOn "\n" used
    return $ searchPort (read s) usedPorts
    where
        searchPort :: Int -> [String] -> Maybe String
        searchPort 1024 _ = Nothing
        searchPort candidate usedPorts = 
            if elem (show candidate) usedPorts
              then searchPort (candidate + 1) usedPorts
              else Just $ show candidate
        isInteger :: String -> Bool
        isInteger s = case reads s :: [(Integer, String)] of
                          [(_, "")] -> True
                          _         -> False
        getProcessOutput :: String -> IO String
        getProcessOutput command = do
              (_pin, pOut, pErr, handle) <- runInteractiveCommand command
              output <- hGetContents pOut
              return output

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

masterJob input slaveProcesses recProc = do
          spawnLocal $ forM_ (zip input (cycle slaveProcesses)) $
            \(m, them) -> send them m
          res <- recProc (length input)
          return res

runSlave host port rtable = do 
                      backend <- initializeBackend host port rtable
                      startSlave backend

mkRecProc "unordered" len = loop len []
                        where
                          loop 0 xs = return xs
                          loop n xs = do
                              x <- expect
                              loop (n-1) (x:xs)

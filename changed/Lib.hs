{-# LANGUAGE TemplateHaskell #-}
module Lib where

import System.Process(runInteractiveCommand, waitForProcess)
import System.Exit (ExitCode)
import System.IO (hGetContents)

import Data.List.Split (splitOn)
import Data.Maybe (isNothing, fromJust)
import Data.List (sortBy)

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forever, forM, forM_)
import Control.Distributed.Process.Serializable

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

mkSlaveJob funcName = remotableDecl [
  [d| slaveJob :: ProcessId -> Process()
      slaveJob them = do
                        forever $ do
                            d <- expect  
                            send them ($(varE funcName) d)

      recOrd :: Maybe String
      recOrd = Just "unordered" |] ]

mkOrderedSlaveJob funcName = remotableDecl [
  [d| slaveJob :: ProcessId -> Process()
      slaveJob them = do
                        forever $ do
                            (i, d) <- expect  :: (Serializable a => (Process (Int, a))) -- (index, data)
                            send them (i, $(varE funcName) d)

      recOrd :: Maybe String
      recOrd = Just "ordered" |] ]

data Format =  H String | P String | R String | L

simpleMS :: String -> String -> Q Exp
simpleMS ms s = gen ms (parse s) [|([], \_ -> True)|]
    where
      parse :: String -> [Format]
      parse [] = []
      parse ('%':'H':xs) = H "" : parse xs
      parse ('%':'P':xs) = P "" : parse xs
      parse ('%':'R':xs) = R "" : parse xs  --receive order 
      parse ('%':'L':xs) = L    : parse xs  --loop 

      gen :: String -> [Format] -> Q Exp -> Q Exp
      gen "master" [] x = [| \input -> \afterFunc -> do
                                  let (h, p, _) = findOptVal (fst $x)
                                  let prop = snd $x
                                  let clos = $(mkClosure (mkName "slaveJob"))
                                  let rtable = $(varE (mkName "__remoteTableDecl")) initRemoteTable 
                                  (host, port) <- getHostIpAndUnusedPort h p
                                  runMaster host port prop $(varE (mkName "recOrd")) rtable clos input afterFunc |] 

      gen "slave" [] x = [| do
                              let (h, p, _) = findOptVal (fst $x)
                              let rtable = $(varE (mkName "__remoteTableDecl")) initRemoteTable 
                              (host, port) <- getHostIpAndUnusedPort h p
                              runSlave host port rtable |]

      gen ms (H _ : xs) x = [| \host -> $(gen ms xs [| (H host:(fst $x), snd $x) |]) |]
      gen ms (P _ : xs) x = [| \port -> $(gen ms xs [| (P port:(fst $x), snd $x) |]) |]
      gen ms (R _ : xs) x = [| \rcvO -> $(gen ms xs [| (R rcvO:(fst $x), snd $x) |]) |]
      gen ms (L   : xs) x = [| \prop -> $(gen ms xs [|        ((fst $x), prop  ) |]) |]
      
findOptVal :: [Format] -> (Maybe String, Maybe String, Maybe String)
findOptVal fs = loop fs (Nothing, Nothing, Nothing)
  where 
    loop [] res = res
    loop (H s:ss) (mh, mp, mr) = loop ss (Just s, mp,     mr)
    loop (P s:ss) (mh, mp, mr) = loop ss (mh,     Just s, mr)
    loop (R s:ss) (mh, mp, mr) = loop ss (mh,     mp,     Just s)
    loop (_  :ss) (mh, mp, mr) = loop ss (mh,     mp,     mr)

getHostIpAndUnusedPort :: Maybe String -> Maybe String -> IO (String, String)
getHostIpAndUnusedPort mh mp = do
                            let host | isNothing mh = "127.0.0.1"
                                     | otherwise    = fromJust mh
                            let port | isNothing mp = "80"
                                     | otherwise    = fromJust mp
                            mp' <- getUnusedPort port
                            case isNothing mp of
                              True  -> return (host, fromJust mp')
                              False -> return (host, port)

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

runMaster host port prop rec rtable clos input afterFunc = do 
          backend <- initializeBackend host port rtable
          startMaster backend $
            \slaves -> do
              us <- getSelfPid
              slaveProcesses <- forM slaves $
                \nid -> spawn nid (clos us)
              res <- loop prop input slaveProcesses rec
              liftIO (afterFunc res)
              where
                loop prop input slaveProcesses rec  = do 
                    res <- masterJob input slaveProcesses rec
                    if prop res
                      then return res
                      else loop prop res slaveProcesses rec

masterJob :: Serializable a => [a] -> [ProcessId] -> Maybe String -> Process [a]
masterJob input slaveProcesses Nothing           = masterJob input slaveProcesses (Just "unordered")
masterJob input slaveProcesses (Just "unordered") = do
          spawnLocal $ forM_ (zip input (cycle slaveProcesses)) $
            \(d, them) -> send them d
          unorderedRecProc (length input)
masterJob input slaveProcesses (Just "ordered") = do
          spawnLocal $ forM_ (zip3 ([1..]::[Int]) input (cycle slaveProcesses)) $
            \(i, d, them) -> send them (i, d)
          orderedRecProc (length input)

runSlave host port rtable = do 
                      backend <- initializeBackend host port rtable
                      startSlave backend

unorderedRecProc :: Serializable a => Int -> Process [a]
unorderedRecProc len = loop len []
                      where
                        loop :: Serializable a => Int -> [a] -> Process [a]
                        loop 0 xs = return xs
                        loop n xs = do
                            d <- expect
                            loop (n-1) (d:xs)
                        
orderedRecProc :: Serializable a => Int -> Process [a]
orderedRecProc len = do 
                      indexedRes <- loop len []
                      let indexedRes' = sortBy (\(i1, _) -> \(i2, _) -> compare i1 i2) indexedRes
                      return $ map (\(_, d) -> d) indexedRes'
                      where
                        loop :: Serializable a => Int -> [(Int, a)] -> Process [(Int, a)]
                        loop 0 xs = return xs
                        loop n xs = do
                            d <- expect
                            loop (n-1) (d:xs)

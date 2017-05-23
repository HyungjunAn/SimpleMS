{-# LANGUAGE TemplateHaskell #-}
module SimpleMS where

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

mkSlaveJob "unordered" funcName = remotableDecl [
  [d| slaveJob :: ProcessId -> Process()
      slaveJob them = do
                    forever $ do
                        d <- expect  
                        send them ($(varE funcName) d)

      recvOrd_ :: String
      recvOrd_ = "unordered" |] ]

mkSlaveJob "ordered" funcName = remotableDecl [
  [d| slaveJob :: ProcessId -> Process()
      slaveJob them = do
              forever $ do
                  (i, d) <- expect :: Serializable a => Process (Int, a) -- (index, data)
                  send them (i, $(varE funcName) d)

      recvOrd_ :: String
      recvOrd_ = "ordered" |] ]

data Format =  H String | P String | L | L1

runMS :: String -> String -> Q Exp
runMS ms s = gen ms (parse s) [| [] |] [| \ x -> x |] [| \_ -> True |]
    where
      parse :: String -> [Format]
      parse [] = []
      parse ('%':'H'    :xs) = H "" : parse xs
      parse ('%':'P'    :xs) = P "" : parse xs
      parse ('%':'L'    :xs) = L    : parse xs  --loop 
      parse ('%':'1':'L':xs) = L1   : parse xs  --loop with meta processing

      gen :: String -> [Format] -> Q Exp -> Q Exp -> Q Exp -> Q Exp
      gen ms (H _:xs) qopts qmproc qprop = [| \host -> $(gen ms xs [| H host: $qopts |] qmproc        qprop   ) |]
      gen ms (P _:xs) qopts qmproc qprop = [| \port -> $(gen ms xs [| P port: $qopts |] qmproc        qprop   ) |]
      gen ms (L  :xs) qopts qmproc qprop = [| \prop -> $(gen ms xs qopts                qmproc      [| prop |]) |]
      gen ms (L1 :xs) qopts qmproc qprop = [| \mproc -> \prop ->
                                                       $(gen ms xs qopts              [| mproc |]   [| prop |]) |]
      gen "master" [] qopts qmproc qprop = [| \input -> \afterFunc -> do
                                  (host, port) <- getHostIpAndUnusedPort $ findHostAndPortOpt $qopts
                                  let prop = $qprop
                                  let mproc = $qmproc
                                  let recvOrd = $(varE (mkName "recvOrd_"))
                                  let rtable = $(varE (mkName "__remoteTableDecl")) initRemoteTable 
                                  let clos = $(mkClosure (mkName "slaveJob"))
                                  runMaster host port mproc prop recvOrd rtable clos input afterFunc |] 
      gen "slave" [] qopts _ _ =
                        [| do (host, port) <- getHostIpAndUnusedPort $ findHostAndPortOpt $qopts
                              let rtable = $(varE (mkName "__remoteTableDecl")) initRemoteTable 
                              runSlave host port rtable |]
      
findHostAndPortOpt :: [Format] -> (Maybe String, Maybe String)
findHostAndPortOpt fs = loop fs (Nothing, Nothing)
  where 
    loop [] res = res
    loop (H s:ss) (mh, mp) = loop ss (Just s, mp    )
    loop (P s:ss) (mh, mp) = loop ss (mh,     Just s)
    loop (_  :ss) (mh, mp) = loop ss (mh,     mp    )

getHostIpAndUnusedPort :: (Maybe String, Maybe String) -> IO (String, String)
getHostIpAndUnusedPort (mh, mp) = do
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

runSlave host port rtable = do 
                      backend <- initializeBackend host port rtable
                      startSlave backend

runMaster host port mproc prop recvOrd rtable clos input afterFunc = do 
          backend <- initializeBackend host port rtable
          startMaster backend $
            \slaves -> do
              us <- getSelfPid
              slaveProcesses <- forM slaves $
                \nid -> spawn nid (clos us)
              res <- loop mproc prop input slaveProcesses recvOrd
              liftIO (afterFunc res)
              where
                loop mproc prop input slaveProcesses recvOrd  = do 
                    res <- masterJob input slaveProcesses recvOrd
                    let res' = mproc res
                    if prop res'
                      then return res'
                      else loop mproc prop res' slaveProcesses recvOrd

masterJob :: Serializable a => [a] -> [ProcessId] -> String -> Process [a]
masterJob input slvPrsss "unordered" = do
          let infos = zip input (cycle slvPrsss)
          spawnLocal $ forM_ infos $
            \(d, them) -> send them d
          recProc "unordered" (length input)
masterJob input slvPrsss "ordered" = do
          let infos = zip3 ([1..]::[Int]) input (cycle slvPrsss)
          spawnLocal $ forM_ infos $
            \(i, d, them) -> send them (i, d)
          recProc "ordered" (length input)

recProc :: Serializable a => String -> Int -> Process [a]
recProc "unordered" len = loop len []
                where
                  loop :: Serializable a => Int -> [a] -> Process [a]
                  loop 0 xs = return xs
                  loop n xs = do
                        d <- expect
                        loop (n-1) (xs ++ [d])
recProc "ordered" len = do 
        indexed <- loop len []
        let sorted = sortBy (\(i1, _) -> \(i2, _) -> compare i1 i2) indexed
        return $ map (\(_, d) -> d) sorted
        where
          loop :: Serializable a => Int -> [(Int, a)] -> Process [(Int, a)]
          loop 0 xs = return xs
          loop n xs = do
                (i, d) <- expect
                loop (n-1) (xs ++ [(i, d)])

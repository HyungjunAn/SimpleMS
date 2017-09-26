{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, BangPatterns, DeriveGeneric #-}

import System.IO
import KMeansCore
import Data.Array
import Data.Array.Unsafe as Unsafe
import Text.Printf
import Data.List
import Data.Function
import Data.Binary (decodeFile)
import System.Environment
import Control.Concurrent
import System.Mem
import Data.Maybe

import PointSum

import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.Vector.Mutable as MVector

import Data.List (sortBy)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Serializable
import Control.Monad
-- -----------------------------------------------------------------------------

initClst :: [Point] -> Int -> [Cluster]
initClst points n =  zip [0 .. n - 1] points


split :: Int -> [a] -> [[a]]
split numChunks xs = chunk (length xs `quot` numChunks) xs

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = as : chunk n bs
  where (as,bs) = splitAt n xs

-- -----------------------------------------------------------------------------

-- <<
assign :: Int -> [Cluster] -> [Point] -> Vector PointSum
assign nclusters clusters points = Vector.create $ do
    vec <- MVector.replicate nclusters (PointSum 0 0 0)
    let
        addpoint p = do
          let c = nearest p; cid = fst c
          ps <- MVector.read vec cid
          MVector.write vec cid $! addToPointSum ps p

    mapM_ addpoint points
    return vec
 where
  nearest p = fst $ minimumBy (compare `on` snd)
                        [ (c, sqDistance (snd c) p) | c <- clusters ]

-- <<addToPointSum
addToPointSum :: PointSum -> Point -> PointSum
addToPointSum (PointSum count xs ys) (x, y)
  = PointSum (count+1) (xs + x) (ys + y)
-- >>

-- <<pointSumToCluster
pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster i (PointSum count xs ys) = (i, (xs / fromIntegral count, ys / fromIntegral count))
-- >>

-- <<addPointSums
addPointSums :: PointSum -> PointSum -> PointSum
addPointSums (PointSum c1 x1 y1) (PointSum c2 x2 y2)
  = PointSum (c1+c2) (x1+x2) (y1+y2)
-- >>

-- <<combine
combine :: Vector PointSum -> Vector PointSum -> Vector PointSum
combine = Vector.zipWith addPointSums
-- >>

-- <<makeNewClusters
makeNewClusters :: Vector PointSum -> [Cluster]
makeNewClusters vec =
  [ pointSumToCluster i ps
  | (i,ps@(PointSum count _ _)) <- zip [0..] (Vector.toList vec)
  , count > 0
  ]

slave :: ProcessId -> Process ()
slave them = do
    forever $ do
        (i, (points, clusters)) <- expect :: Process (Int, ([Point], [Cluster]))
        let vec = assign (length clusters) clusters points
        let ret = Vector.toList vec
        send them (i, (points, ret))

remotable ['slave]

tooMany = 300

kmeans_strat_cloud :: Int -> [Point] -> [Cluster] -> [NodeId] -> Process [Cluster]
kmeans_strat_cloud numChunks points clusters slaves =
  let
      chunks = split numChunks points                            -- <1>
      pAndCs = map (\x -> (x, clusters)) chunks
 
      loop :: Int -> [([Point], [Cluster])] -> [ProcessId] -> Process [Cluster]
      loop n pAndCs sp | n > tooMany = do
        liftIO $ printf "giving up.\n"
        return (snd (pAndCs !! 0))
      loop n pAndCs sp = do
        --liftIO $ printf $ (show n) ++ "\n"
        res <- masterKmeans pAndCs sp -- <2>
        if snd (res !! 0) == snd (pAndCs !! 0)
           then return (snd (pAndCs !! 0))
           else loop (n+1) res sp
  in do
    us <- getSelfPid
    slaveProcesses <- forM slaves $
        \nid -> Control.Distributed.Process.spawn nid ($(mkClosure 'slave) us)
    loop 0 pAndCs slaveProcesses

masterKmeans :: [([Point], [Cluster])] -> [ProcessId] -> Process [([Point], [Cluster])]
masterKmeans pointssAndClusters slaveProcesses = do
    spawnLocal $ forM_ (zip3 ([1..]::[Int]) pointssAndClusters (cycle slaveProcesses)) $
        \(i, (points, clusters), them) -> send them (i, (points, clusters))
    cls <- sumPoint (length pointssAndClusters)
    let sorted = sortBy (\(i1, _) -> \(i2, _) -> compare i1 i2) cls
    let ret    = map (\x -> snd x) sorted
    let ret'   = map (\(_, x) -> Vector.fromList x) ret
    let cls' = makeNewClusters $ foldr1 combine $ ret'
    let res = map (\(x, _) -> (x, cls')) ret
    return res

sumPoint = go []
    where
        go !xs 0 = return xs
        go !xs n =do
            (i, x) <- expect :: Process (Int, ([Point], [PointSum]))
            go (xs ++ [(i, x)]) (n-1)

rtable = __remoteTable initRemoteTable

main = runInUnboundThread $ do
  args <- getArgs
  case args of
    ["master", host, port, f] -> do
        points <- decodeFile f
        let clusters 	= initClst points 5
        let nclusters = length clusters
        performGC
        mapM_ print clusters
        backend <- initializeBackend host port rtable
        startMaster backend $ \slaves -> do
            final_clusters <- kmeans_strat_cloud 64 points clusters slaves
            liftIO $ mapM_ print final_clusters
    ["slave", host, port] -> do
        backend <- initializeBackend host port rtable
        startSlave backend


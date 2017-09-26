{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, BangPatterns, DeriveGeneric #-}

import System.IO
import Data.Array
import Data.Array.Unsafe as Unsafe
import Text.Printf
import Data.List
import Data.Function
import Data.Binary (decodeFile)
import Debug.Trace
import System.Environment
import Control.Concurrent

import KMeansCore
import PointSum

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector

import SimpleMS
-- -----------------------------------------------------------------------------

initClst :: [Point] -> Int -> [Cluster]
initClst points n =  zip [0 .. n - 1] points

split :: Int -> [a] -> [[a]]
split numChunks xs = chunk (length xs `quot` numChunks) xs

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = as : chunk n bs
  where (as,bs) = splitAt n xs

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


addToPointSum :: PointSum -> Point -> PointSum
addToPointSum (PointSum count xs ys) (x, y)
  = PointSum (count+1) (xs + x) (ys + y)

pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster i (PointSum count xs ys) = (i, (xs / fromIntegral count, ys / fromIntegral count))

addPointSums :: PointSum -> PointSum -> PointSum
addPointSums (PointSum c1 x1 y1) (PointSum c2 x2 y2)
  = PointSum (c1+c2) (x1+x2) (y1+y2)

combine :: Vector PointSum -> Vector PointSum -> Vector PointSum
combine = Vector.zipWith addPointSums

makeNewClusters :: Vector PointSum -> [Cluster]
makeNewClusters vec =
  [ pointSumToCluster i ps
  | (i,ps@(PointSum count _ _)) <- zip [0..] (Vector.toList vec)
  , count > 0
  ]
  
func :: ([Point], [Cluster]) -> ([Point], [PointSum])
func (points, clusters) = (points, Vector.toList vec) 
    where
      vec = assign (length clusters) clusters points

mkSlaveJob "ordered" 'func

metaProc ret = map (\(x, _) -> (x, cls)) ret
    where 
      cls = makeNewClusters $ foldr1 combine $ ret'
      ret'   = map (\(_, x) -> Vector.fromList x) ret

proc pAndcs1 pAndcs2 = snd (pAndcs1 !! 0) == snd (pAndcs2 !! 0)

afterFunc pAndcs = mapM_ print (snd (pAndcs !! 0))

main = runInUnboundThread $ do
  args <- getArgs
  case args of
    ["master", host, port, f] -> do
        points <- decodeFile f
        let clusters 	= initClst points 5
        let nclusters = length clusters
        let chunks = split 64 points
        let pAndCs = map (\x -> (x, clusters)) chunks
        mapM_ print clusters
        $(runMS "master" "%H%P%2L") host port metaProc proc pAndCs afterFunc
    ["slave", host, port] -> do
        $(runMS "slave"  "%H%P")    host port


{-# LANGUAGE TemplateHaskell #-}

module Main where
import SimpleMS
import Data.List.Split

import System.Environment (getArgs)

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

afterFunc :: [[(String, Int)]] -> IO ()
afterFunc xs = putStrLn $ show $ findMax $ count (xs !! 0)

prop xs = length xs == 1
------------------------------------------------

mkSlaveJob "unordered" 'count

main = do 
  args <- getArgs
  case args of
    ["master", host, port, f] -> do
        content <- readFile f
        let input''  = lines content
            input'   = zip input'' ([1, 1..] :: [Int])
            input    = chunksOf 10 input'

        $(runMS "master" "%H%P%1L") host port (merge 3) prop input afterFunc
    ["slave",  host, port] -> do
        $(runMS "slave" "%H%P") host port

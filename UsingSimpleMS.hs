{-# LANGUAGE TemplateHaskell #-}
module UsingSimpleMS where
import System.Environment (getArgs)

import SimpleMS

incr :: (Int, Int) -> (Int, Int)
incr (x, y) = (x + 1, y + 1)

sqr :: Int -> Int
sqr x = x ^ 2

input = [(1, 2), (3, 4), (5, 6)] :: [(Int, Int)]


prop :: Int -> [Int] -> Bool
prop n xs | sum xs > n  = True
          | otherwise   = False

afterFunc :: [(Int, Int)] -> IO()
afterFunc xs = putStrLn $ show $ xs
--afterFunc xs = putStrLn $ show xs
--------------------------------------------------------------------------
--mkSlaveJob 'sqr
--mkSlaveJob 'incr
--mkSlaveJob 'incr
--mkSlaveJob 'incr
mkSlaveJob "ordered" 'incr

main = do
  args <- getArgs
  case args of
    ["master", host, port] ->
      $(runMS "master" "%H%P%1L") host port (map (\(x, y) -> (y, x))) (\ xs -> fst (xs !! 0) == 3) input afterFunc 
    ["slave",  host, port] -> 
      $(runMS "slave" "%H%P") host port 
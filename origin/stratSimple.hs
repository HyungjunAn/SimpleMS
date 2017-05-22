import Control.Parallel.Strategies as Strategies

input::[Int]
input = [1, 2, 3, 4]

incr :: Int -> Int
incr x = x + 1

output  = map incr input `using` parList rseq
output' = sum output

main = putStrLn $ show output'

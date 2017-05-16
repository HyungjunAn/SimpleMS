input::[Int]
input = [1, 2, 3, 4]

incr :: Int -> Int
incr x = x + 1

output  = map incr input -- :: [Int]
output' = sum output     -- :: Int

main = putStrLn $ show output'

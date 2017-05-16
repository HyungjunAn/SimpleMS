main :: IO Int
main = do
      str <- getLine 
      let n = read str + 1
      putStrLn (show n)
      return n


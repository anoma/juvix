-- count combinations of numbers 1 to N having sum N

combinations :: Int -> Int
combinations n = go n n
  where
    go :: Int -> Int -> Int
    go n 0 = 1
    go 0 s = 0
    go n s = if s <= 0 then 0 else go n (s - n) + go (n - 1) s

main :: IO ()
main = putStrLn (show (combinations 100))

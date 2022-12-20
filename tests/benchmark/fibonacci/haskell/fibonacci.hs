-- compute the Nth Fibonacci number modulo 2^28

fib :: Int -> Int
fib n = go n 0 1
  where
    go :: Int -> Int -> Int -> Int
    go 0 n m = n
    go k n m = go (k - 1) m ((n + m) `mod` (2 ^ 28))

main :: IO ()
main = putStrLn (show (fib 1000000))

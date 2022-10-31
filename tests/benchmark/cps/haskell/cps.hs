-- compute the Nth Fibonacci number modulo 2^28 with CPS

go :: Int -> Int -> Int -> Int
go k n m = step k n m go

step :: Int -> Int -> Int -> (Int -> Int -> Int -> Int) -> Int
step 0 n m cont = n
step k n m cont = cont (k - 1) m ((n + m) `mod` (2 ^ 28))

fib :: Int -> Int
fib n = go n 0 1

main :: IO ()
main = putStrLn (show (fib 100000000))

-- fold a list of N integers

import Data.List

plusMod :: Int -> Int -> Int
plusMod x y = (x + y) `mod` (2 ^ 28)

run :: Int -> Int -> [Int] -> Int
run 0 acc lst = foldl' plusMod acc lst
run n acc lst = run (n - 1) (foldl' plusMod acc lst) lst

main :: IO ()
main = putStrLn (show (run k 0 [1 .. n]))
  where
    k = 1000
    n = 100000

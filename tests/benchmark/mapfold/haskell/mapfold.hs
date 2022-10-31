-- map and fold a list of N integers K times

import Data.List

run :: Int -> Int -> [Int] -> Int
run 0 acc lst = acc
run n acc lst = run (n - 1) (foldl' (+) (-acc) lst) (map (+ 1) lst)

main :: IO ()
main = putStrLn (show (run k 0 [1..n]))
  where
    k = 10000
    n = 10000

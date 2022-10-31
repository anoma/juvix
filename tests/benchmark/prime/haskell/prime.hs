-- Compute the Nth prime number

import Data.List

prime :: Int -> Int
prime n = go 0 2 []
  where
    go :: Int -> Int -> [Int] -> Int
    go k p lst =
      if k == n then
        head lst
      else if checkDivisible p lst then
        go k (p + 1) lst
      else
        go (k + 1) (p + 1) (p : lst)

    checkDivisible :: Int -> [Int] -> Bool
    checkDivisible p [] = False
    checkDivisible p (h : _) | p `mod` h == 0 = True
    checkDivisible p (_ : t) = checkDivisible p t

main :: IO ()
main = putStrLn (show (prime (1024 * 16)))

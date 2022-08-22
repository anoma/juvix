
import Data.List

eratostenes :: [Integer] -> [Integer]
eratostenes (h : t) = h : eratostenes (filter (\x -> x `mod` h /= 0) t)

primes :: [Integer]
primes = eratostenes [2..]

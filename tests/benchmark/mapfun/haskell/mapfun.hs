-- successively map K functions to a list of N integers

mapfun :: [a -> a] -> [a] -> [a]
mapfun [] xs = xs
mapfun (f : fs) xs = mapfun fs (map f xs)

genfs :: Int -> [Int -> Int]
genfs 0 = []
genfs n = ((-) n) : genfs (n - 1)

genffs :: Int -> [(Int -> Int) -> Int -> Int]
genffs 0 = []
genffs n = (\f x -> f (x + n)) : genffs (n - 1)

sumi :: [Int] -> Int
sumi [] = 0
sumi (x : xs) = x + sumi xs

main :: IO ()
main = putStrLn (show (sumi (mapfun (mapfun (genffs k) (genfs k)) [1 .. n])))
  where
    k = 100
    n = 10000

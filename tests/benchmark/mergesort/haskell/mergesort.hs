-- Merge sort a list of N integers

split :: [a] -> ([a], [a])
split xs = go xs [] []
  where
    go [] ys zs = (ys, zs)
    go (x : xs') ys zs = go xs' zs (x : ys)

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x : xs') ys@(y : ys') | x <= y = x : merge xs' ys
merge xs (y : ys') = y : merge xs ys'

sort :: [Int] -> [Int]
sort [] = []
sort [x] = [x]
sort xs =
  let (l1, l2) = split xs
   in merge (sort l1) (sort l2)

sorted :: [Int] -> Bool
sorted [] = True
sorted [_] = True
sorted (x : y : t) | x <= y = sorted (y : t)
sorted _ = False

gen :: Int -> [Int] -> [Int]
gen 0 acc = acc
gen n acc = gen (n - 1) (n : acc)

main :: IO ()
main = putStrLn (show (sorted (sort (gen 2000000 []))))

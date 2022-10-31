-- optionally sum N integers in a binary tree K times

data Tree = Node Int Tree Tree | Leaf

gen :: Int -> Tree
gen 0 = Leaf
gen n = Node n t t
  where
    t = gen (n - 1)

sumi :: Int -> Tree -> Maybe Int
sumi _ Leaf = Just 0
sumi x (Node y _ _) | x == y = Nothing
sumi x (Node y l r) = do
  s1 <- sumi x l
  s2 <- sumi x r
  return (s1 + s2 + y)

run :: Int -> Tree -> Maybe Int
run 0 t = sumi 0 t
run n t =
  case run (n - 1) t of
    Nothing -> sumi n t
    Just x -> do
      y <- sumi n t
      return (y - x)

main :: IO ()
main = putStrLn (show (run 101 (gen 20)))

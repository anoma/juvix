data Tree = Leaf | Node Tree Tree

gen :: Int -> Tree
gen n = if n <= 0 then Leaf else Node (gen (n - 2)) (gen (n - 1))

f :: Tree -> Integer
f Leaf = 1
f (Node l r) =
  let l' = g l
   in let r' = g r
       in let a = case l' of
                Leaf -> -3
                Node l r -> f l + f r
           in let b = case r' of
                    Node l r -> f l + f r
                    _ -> 2
               in a * b

isNode :: Tree -> Bool
isNode (Node _ _) = True
isNode Leaf = False

isLeaf :: Tree -> Bool
isLeaf Leaf = True
isLeaf _ = False

g :: Tree -> Tree
g t =
  if isLeaf t
    then t
    else case t of
      Node l r -> if isNode l then r else Node r l

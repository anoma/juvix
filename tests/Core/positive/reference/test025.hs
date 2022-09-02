f :: Integer -> Integer
f x =
  if x < 1 then
    1
  else
    2 * x + g (x - 1)

g :: Integer -> Integer
g x =
  if x < 1 then
    1
  else
    x + h (x - 1)

h :: Integer -> Integer
h x =
  if x < 1 then
    1
  else
    x * f (x - 1)

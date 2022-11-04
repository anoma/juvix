-- Ackermann function (higher-order definition)

compose :: (a -> b) -> (b -> c) -> a -> c
compose f g x = g (f x)

iter :: (a -> a) -> Int -> a -> a
iter f 0 = id
iter f n = compose f (iter f (n - 1))

ackermann :: Int -> Int -> Int
ackermann m =
  iter (\f n -> iter f (n + 1) 1) m (+ 1)

main :: IO ()
main = putStrLn (show (ackermann 3 11))

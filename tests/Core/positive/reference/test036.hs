
power :: Integer -> Int -> Integer
power x y = power' x y 1
  where
    power' x y acc =
      if y == 0 then
        acc
      else
        power' x (y - 1) (x * acc)

num1 :: Integer
num1 = 1267650600228229401496703205376

num2 :: Integer
num2 = -126765060022822940149670320537674809325432

msqrt :: Integer -> Integer
msqrt x = sqrt' x (x + 1) 0
  where
    sqrt' x top bot =
      if top - bot <= 1 then
        bot
      else
        let y = (top + bot) `div` 2
        in
          if y * y > x then
            sqrt' x y bot
          else
            sqrt' x top y

dlog :: Integer -> Integer -> Integer
dlog x y = log' x y 0
  where
    log' x y acc =
      if y == 1 then
        acc
      else
        log' x (y `div` x) (acc + 1)

fast_power :: Integer -> Int -> Integer
fast_power x y = fast_power' x y 1
  where
    fast_power' x y acc =
      if y == 0 then
        acc
      else if y `mod` 2 == 1 then
        fast_power' (x * x) (y `div` 2) (x * acc)
      else
        fast_power' (x * x) (y `div` 2) acc

main :: IO ()
main = do
  putStrLn $ show num1
  putStrLn $ show num2
  putStrLn $ show (num1 + num2)
  putStrLn $ show (num1 - num2)
  putStrLn $ show (num1 * num2)
  putStrLn $ show (num2 `div` num1)
  putStrLn $ show (num2 `mod` num1)
  putStrLn ""

  putStrLn $ show (power 2 30)
  putStrLn $ show (power 2 31)
  putStrLn $ show (power 2 32)
  putStrLn $ show (power 2 62)
  putStrLn $ show (power 2 63)
  putStrLn $ show (power 2 64)
  putStrLn $ show (power 2 100)
  putStrLn $ show (power (-2) 30)
  putStrLn $ show (power (-2) 31)
  putStrLn $ show (power (-2) 32)
  putStrLn $ show (power (-2) 62)
  putStrLn $ show (power (-2) 63)
  putStrLn $ show (power (-2) 64)
  putStrLn $ show (power (-2) 101)
  putStrLn ""

  putStrLn $ show (msqrt (power 2 128))
  putStrLn $ show (msqrt (power 3 180))
  putStrLn $ show (msqrt (power num1 2))
  putStrLn $ show (msqrt (power num2 2))
  putStrLn ""

  putStrLn $ show (dlog 2 (power 2 100))
  putStrLn $ show (dlog 20 (power 20 100))
  putStrLn $ show (dlog 9 (power 3 100))
  putStrLn ""

  putStrLn $ show (fast_power 2 30)
  putStrLn $ show (fast_power 2 31)
  putStrLn $ show (fast_power 2 32)
  putStrLn $ show (fast_power 2 62)
  putStrLn $ show (fast_power 2 63)
  putStrLn $ show (fast_power 2 64)
  putStrLn $ show (fast_power 2 100)
  putStrLn $ show (fast_power (-2) 30)
  putStrLn $ show (fast_power (-2) 31)
  putStrLn $ show (fast_power (-2) 32)
  putStrLn $ show (fast_power (-2) 62)
  putStrLn $ show (fast_power (-2) 63)
  putStrLn $ show (fast_power (-2) 64)
  putStrLn $ show (fast_power (-2) 101)
  putStrLn ""

  putStrLn $ show (fast_power 2 1000)
  putStrLn $ show (fast_power 3 1000)
  putStrLn $ show (fast_power 2 10000)
  putStrLn $ show (fast_power 2 100000)

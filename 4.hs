import Data.Char

pyths n
  = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x ^ 2 + y ^ 2 == z ^ 2]

factors n
  = [ x |  x <- [1..n], n `mod` x == 0]

perfects n
  = [x | x <- [1..n], isPerfect x]
    where isPerfect num = sum (init (factors num)) == num

find k t = [v | (k', v) <- t, k == k']

positions x xs = find x (zip xs [0..n])
  where n = length xs - 1

scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]

let2int c = ord c - ord 'a'

int2let n = chr (ord 'a' + n)

shift n c
  | isUpper c = shift n (toLower c)
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode n xs = [shift n x | x <- xs]

equation xs = 1 : [x + 1 | x <- xs]

riffle xs ys = concat [[x, y] | (x, y) <- xs `zip` ys]

divides x y = x `mod` y == 0

divisors n = [x | x <- [1..n], n `divides` x]

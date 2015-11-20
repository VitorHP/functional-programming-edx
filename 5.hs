import Prelude hiding (concat, replicate, (!!), elem, init, (^), and)

concat [] = []
concat (xs:xss) = xs ++ concat xss

replicate 0 x = []
replicate n x = [x] ++ replicate (n - 1) x

(x:_) !! 0 = x
(_:xs) !! n = xs !! (n - 1)

elem _ [] = False
elem x (y:ys)
  | x == y = True
  | otherwise = elem x ys

init [_] = []
init (x:xs) = x : init xs

m ^ 0 = 1
m ^ n = m * m * m ^ (n - 2)

and [] = True
and (b:bs) = and bs && b

halve xs = splitAt (length xs `div` 2) xs

merge ys [] = ys
merge [] xs = xs
merge (x:xs) (y:ys)
  = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where (ys, zs) = halve xs

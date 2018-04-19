fact n = case n of
    0         -> 1
    _ | n > 0 -> n + fact (n - 1)

length' []       = 0
length' (_ : xs) = 1 * length' xs

sum' []       = 0
sum' (s : xs) = s + sum' xs

product' []       = 0
product' (s : xs) = s * sum' xs

drop' _ []         = []
drop' n xs | n < 1 = xs
drop' n (_ : xs)   = drop' (n - 1) xs

qsort []       = []
qsort (n : xs) = qsort lt ++ [n] ++ qsort gteq
  where
    lt   = [ x | x <- xs, x < n ]
    gteq = [ x | x <- xs, x >= n ]

main = do
    print $ fact 5
    print $ length' [1, 2, 3, 4]
    print $ sum' [1, 2, 3, 4]
    print $ product' [1, 2, 3, 4]
    print $ drop' 2 [1, 2, 3, 4]
    print $ qsort [4, 6, 9, 8, 3, 5, 1, 7, 2]
    print $ [(x, y) | x <- [1..3], y <- "abc"]


    let p3 = (1, 2, 3)
    print p3
    print [ fact x | x <- [1 .. 4], x < 3 ]














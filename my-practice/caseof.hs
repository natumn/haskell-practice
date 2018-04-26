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

data Color = Blue | Red | Green | Yellow deriving (Show, Enum)
data Point = Point Int Int deriving Show

offset (Point x1 x2) (Point x3 x4) = Point (x1 + x2) (x3 + x4)

data Rect = Rect Int Int Int Int
          | Rect3D Int Int Int Int Int Int
          deriving Show

contains (Rect x y z w) (Point p1 p2) =
    x <= p1 && p1 < x + w && y <= p2 && p2 < y + w

main = do
    print $ fact 5
    print $ length' [1, 2, 3, 4]
    print $ sum' [1, 2, 3, 4]
    print $ product' [1, 2, 3, 4]
    print $ drop' 2 [1, 2, 3, 4]
    print $ qsort [4, 6, 9, 8, 3, 5, 1, 7, 2]
    print $ [ (x, y) | x <- [1 .. 3], y <- "abc" ]


    let p3 = (1, 2, 3)
    print p3
    print [ fact x | x <- [1 .. 4], x < 3 ]
    print Blue
    print $ fromEnum Green
    print (toEnum 2 :: Color)
    let a = Point 2 3
        b = Point 1 1
        c = offset a b
    print c
    print $ contains (Rect 2 2 3 3) (Point 2 2)

























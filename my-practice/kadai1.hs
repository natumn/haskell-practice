import qualified Data.Set                      as Set

isSubSet :: Int -> Int -> Bool
isSubSet x y = Set.isSubsetOf x y

main = do
    let a = Set.fromList [1, 2, 3, 4, 5]
        b = Set.fromList [4, 5, 6, 7]


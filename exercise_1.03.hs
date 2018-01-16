
-- Exercise 1.3.
-- Define a procedure that takes three numbers as arguments
-- and returns the sum of the squares of the two larger numbers.

import Data.List

cmp1 :: Int -> Int -> Ordering 
cmp1 a b
    | a < b = GT
    | a > b = LT
    | otherwise = EQ

square1 :: Int -> Int
square1 a = a * a

sqsum1 :: Int -> Int -> Int -> Int
sqsum1 a b c = res where
    list = [a, b, c]
    sorted = sortBy cmp1 list
    larger = take 2 sorted
    squared = fmap square1 larger
    res = foldr (+) 0 squared


main :: IO ()
main = do
    print $ sqsum1 1 2 3
    print $ sqsum1 3 2 1
    print $ sqsum1 3 1 2
    print $ sqsum1 1 3 2
    print $ sqsum1 2 1 3
    

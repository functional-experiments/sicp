
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

square2Larger :: [Int] -> [Int]
square2Larger list = fmap square1 (take 2 sorted)
   where sorted = sortBy cmp1 list

sqsum1 :: Int -> Int -> Int -> Int
sqsum1 a b c = foldr (+) 0 squared
    where squared = square2Larger [a, b, c]


main :: IO ()
main = do
    print $ sqsum1 1 2 3
    print $ sqsum1 3 2 1
    print $ sqsum1 3 1 2
    print $ sqsum1 1 3 2
    print $ sqsum1 2 1 3
    

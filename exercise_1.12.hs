
-- Exercise 1.12.
--
-- The following pattern of numbers is called Pascal's triangle.
-- https://mitpress.mit.edu/sicp/full-text/book/ch1-Z-G-17.gif
-- The numbers at the edge of the triangle are all 1, and each number
-- inside the triangle is the sum of the two numbers above it.
-- Write a procedure that computes elements of Pascal's triangle
-- by means of a recursive process. 

nextAdd :: [Int] -> Int -> Int
nextAdd list idx
    | length list - 1 == idx = list !! idx
    | otherwise = list !! idx + list !! (idx + 1)

ptRow :: [Int] -> [Int]
ptRow prevRow = 1 : fmap (\idx -> nextAdd prevRow idx) [0 .. length prevRow - 1]

ptTriangle :: [[Int]] -> Int -> [[Int]]
ptTriangle triangle n
    | 0 == n = reverse triangle
    | otherwise =  ptTriangle withNextRow (n - 1)
    where
        withNextRow = ptRow (head triangle) : triangle


main :: IO ()
main = do
    print $ ptTriangle [[1]] 7


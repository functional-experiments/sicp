
-- Exercise 1.11.
-- A function f is defined by the rule that
--
-- f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>=3 .
--
-- Write a procedure that computes f by means of a recursive process.
-- Write a procedure that computes f by means of an iterative process. 

f1 :: Int -> Int
f1 n
    | n < 3 = n
    | otherwise = f1 (n - 1) + 2 * f1 (n - 2) + 3 * f1 (n - 3)

f2Iter :: Int -> Int -> Int -> Int -> Int
f2Iter a b c n
    | n < 3 = a
    | otherwise = f2Iter (a + 2 * b + 3 * c) a b (n - 1)

f2 :: Int -> Int
f2 n
    | n < 3 = n
    | otherwise = f2Iter 2 1 0 n


main :: IO ()
main = do
    print $ "recursive"
    print $ "f(2)"
    print $ f1 2
    print $ "f(4)"
    print $ f1 4
    print $ "f(16)"
    print $ f1 16 

    print $ "iterative"
    print $ "f(2)"
    print $ f2 2
    print $ "f(4)"
    print $ f2 4
    print $ "f(16)"
    print $ f2 16 


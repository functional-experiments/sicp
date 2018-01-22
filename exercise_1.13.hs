
-- Exercise 1.13.
--
-- Prove that Fib(n) is the closest integer to n/5, where = (1 + 5)/2.
-- Hint: Let = (1 - 5)/2. Use induction and the definition of the
-- Fibonacci numbers (see section 1.2.2) to prove that Fib(n) = (n - n)/5. 

-- (define (fib-iter a b count)
--  (if (= count 0)
--      b
--      (fib-iter (+ a b) a (- count 1))))
fibIter :: Int -> Int -> Int -> Int
fibIter a b count
    | count > 0 = fibIter (a + b) a (count - 1)
    | otherwise = b

-- (define (fib n)
--  (fib-iter 1 0 n))
fib :: Int -> Int
fib n = fibIter 1 0 n

fib2 :: Int -> Float
fib2 n = (((1 + sqrt 5) / 2) ^^ n) / sqrt 5


main :: IO ()
main = do
    print $ fib 7
    print $ fib2 7

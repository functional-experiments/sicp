
-- Exercise 1.10.
-- The following procedure computes a mathematical function called
-- Ackermann's function.
--
-- (define (A x y)
--   (cond ((= y 0) 0)
--         ((= x 0) (* 2 y))
--         ((= y 1) 2)
--         (else (A (- x 1)
--                  (A x (- y 1))))))

a1 :: Int -> Int -> Int
a1 x y 
    | 0 == y = 0
    | 0 == x = (*) 2 y
    | 1 == y = 2
    | otherwise = a1 (x - 1) (a1 x (y - 1))
        
-- Consider the following procedures, where A is the procedure defined above:
--
-- (define (f n) (A 0 n))
f :: Int -> Int
f n = a1 0 n

-- (define (g n) (A 1 n))
g :: Int -> Int
g n = a1 1 n

-- (define (h n) (A 2 n))
h :: Int -> Int
h n = a1 2 n

-- (define (k n) (* 5 n n))
k :: Int -> Int
k n = foldr (*) 1 [5, n, n]


main :: IO ()
main = do
-- What are the values of the following expressions?
    print $ "(A 1 10)"
    print $ a1 1 10
    print $ "(A 2 4)"
    print $ a1 2 4 
    print $ "(A 3 3)"
    print $ a1 3 3 

-- Give concise mathematical definitions for the functions computed by the
-- procedures f, g, and h for positive integer values of n.
-- For example, (k n) computes 5n2. 
    putStrLn ""
    print $ "f(n) = 2n"
    print $ "(f 2)"
    print $ f 2
    print $ "(f 3)"
    print $ f 3
    print $ "(f 4)"
    print $ f 4

    putStrLn ""
    print $ "f(n) = 2^n"
    print $ "(g 2)"
    print $ g 2
    print $ "(g 3)"
    print $ g 3
    print $ "(g 10)"
    print $ g 10

    putStrLn ""
    print $ "h(n) = 2^(2^...[n-1])"
    print $ "(h 2)"
    print $ h 2
    print $ "(h 3)"
    print $ h 3
    print $ "(h 4)"
    print $ h 4



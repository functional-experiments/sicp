
-- Exercise 1.16.
--
-- Design a procedure that evolves an iterative exponentiation process that
-- uses successive squaring and uses a logarithmic number of steps,
-- as does fast-expt.
--
-- (Hint: Using the observation that (bn/2)2 = (b2)n/2, keep, along with
-- the exponent n and the base b, an additional state variable a,
-- and define the state transformation in such a way that the product
-- a bn is unchanged from state to state.
-- At the beginning of the process a is taken to be 1, and the answer is given
-- by the value of a at the end of the process. In general, the technique of
-- defining an invariant quantity that remains unchanged from state to state is
-- a powerful way to think about the design of iterative algorithms.) 

-- (define (even? n)
--   (= (remainder n 2) 0))
isEven :: Int -> Bool
isEven n = 0 == rem n 2

square :: Int -> Int
square n = n * n

-- (define (fast-expt b n)
--   (cond ((= n 0) 1)
--         ((even? n) (square (fast-expt b (/ n 2))))
--         (else (* b (fast-expt b (- n 1))))))
fastExptRecursive :: Int -> Int -> Int
fastExptRecursive b n 
    | 0 == n = 1
    | isEven n = square (fastExptRecursive b (div n 2))
    | otherwise = b * fastExptRecursive b (n - 1)

fastExptIter :: Int -> Int -> Int -> Int
fastExptIter a b n
    | 0 == n = a
    | isEven n = fastExptIter a (square b) (div n 2)
    | otherwise = fastExptIter (a * b) b (n - 1)


main :: IO ()
main = do
    print $ isEven 4
    print $ isEven 5
    print $ fastExptRecursive 2 10
    print $ fastExptRecursive 2 11
    print $ fastExptIter 1 2 10
    print $ fastExptIter 1 2 11

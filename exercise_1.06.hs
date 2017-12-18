
-- Exercise 1.6.
-- Alyssa P. Hacker doesn't see why if needs to be provided as a special form.
-- `Why can't I just define it as an ordinary procedure in terms of cond?'
-- she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done,
-- and she defines a new version of if:
--
-- (define (new-if predicate then-clause else-clause)
--   (cond (predicate then-clause)
--       (else else-clause)))
--
-- Delighted, Alyssa uses new-if to rewrite the square-root program:
--
-- (define (sqrt-iter guess x)
--   (new-if (good-enough? guess x)
--       guess
--       (sqrt-iter (improve guess x)
--          x)))
--
-- What happens when Alyssa attempts to use this to compute square roots?
-- Explain. 

abs1 :: Float -> Float
abs1 a 
    | a >= 0 = a
    | otherwise = (-a)

square1 :: Float -> Float
square1 a = a * a

-- (define (good-enough? guess x)
--   (< (abs (- (square guess) x)) 0.001))
goodEnough :: Float -> Float -> Bool
goodEnough guess x  = abs1(square1(guess) - x) < 0.001

-- (define (average x y)
--   (/ (+ x y) 2))
average :: Float -> Float -> Float
average x y = (x + y) / 2

-- (define (improve guess x)
--   (average guess (/ x guess)))
improve :: Float -> Float -> Float
improve guess x = average guess (x / guess)

-- (define (sqrt-iter guess x)
--   (if (good-enough? guess x)
--       guess
--       (sqrt-iter (improve guess x)
--                  x)))
sqrtIter :: Float -> Float -> Float
sqrtIter guess x
    | goodEnough guess x = guess
    | otherwise = sqrtIter (improve guess x) x

-- (define (new-if predicate then-clause else-clause)
--   (cond (predicate then-clause)
--       (else else-clause)))
newIf :: Bool -> Float -> Float -> Float
newIf predicate thenClause elseClause
    | predicate = thenClause
    | otherwise = elseClause

-- (define (sqrt-iter guess x)
--   (new-if (good-enough? guess x)
--       guess
--       (sqrt-iter (improve guess x)
--          x)))
newSqrtIter :: Float -> Float -> Float
newSqrtIter guess x =
    newIf (goodEnough guess x)
        guess
        (newSqrtIter (improve guess x) x)


main :: IO () 
main = do
    print $ sqrtIter 1 2

    print "(new-if (= 2 3) 0 5)"
    print $ newIf (2 == 3) 0 5

    print "(new-if (= 1 1) 0 5)"
    print $ newIf (1 == 1) 0 5

    print $ newSqrtIter 1 2


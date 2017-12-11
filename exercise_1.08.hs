
-- Exercise 1.8.
-- Newton's method for cube roots is based on the fact that if y is an
-- approximation to the cube root of x, then a better approximation
-- is given by the value
--
-- (x/y^2) + 2y
-- ------------
--      3
--
-- Use this formula to implement a cube-root procedure analogous to the square-root procedure.

abs1 :: Float -> Float
abs1 a = res where
    res | (>=) a 0 = a
        | otherwise = (-a)

cube1 :: Float -> Float
cube1 a = foldr (*) 1 [a, a, a]

-- (define (good-enough? guess x)
--   (< (abs (- (square guess) x)) 0.001))
goodEnough :: Float -> Float -> Bool
goodEnough guess x = res where
    res = (<)
        (abs1 ((-) (cube1 guess) x))
        0.001

-- (define (average x y)
--   (/ (+ x y) 2))
average :: Float -> Float -> Float
average x y = res where
    res = (/) ((+) x y) 2

-- (define (improve guess x)
--   (average guess (/ x guess)))
improve :: Float -> Float -> Float
improve guess x = res where
    approx = (/)
        ((+)
            ((/) x ((*) guess guess))
            ((*) 2 guess))
        3
    res = average guess approx 

-- (define (sqrt-iter guess x)
--   (if (good-enough? guess x)
--       guess
--       (sqrt-iter (improve guess x)
--                  x)))
cubeIter :: Float -> Float -> Float
cubeIter guess x = res where
    res = if goodEnough guess x
        then guess
        else cubeIter (improve guess x) x


main :: IO () 
main = do
    print $ cubeIter 1 2


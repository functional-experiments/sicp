
-- Exercise 1.7.
-- The good-enough? test used in computing square roots will not be very
-- effective for finding the square roots of very small numbers.
-- Also, in real computers, arithmetic operations are almost always performed
-- with limited precision. This makes our test inadequate for very large
-- numbers. Explain these statements, with examples showing how the test
-- fails for small and large numbers. An alternative strategy for implementing
-- good-enough? is to watch how guess changes from one iteration to the next
-- and to stop when the change is a very small fraction of the guess.
-- Design a square-root procedure that uses this kind of end test. Does
-- this work better for small and large numbers? 

abs1 :: Float -> Float
abs1 a = res where
    res | (>=) a 0 = a
        | otherwise = (-a)

square1 :: Float -> Float
square1 a = (*) a a

-- (define (average x y)
--   (/ (+ x y) 2))
average :: Float -> Float -> Float
average x y = res where
    res = (/) ((+) x y) 2

-- (define (improve guess x)
--   (average guess (/ x guess)))
improve :: Float -> Float -> Float
improve guess x = res where
    res = average guess ((/) x guess)

-- (define (good-enough? guess x)
--   (< (abs (- (square guess) x)) 0.001))
goodEnough0 :: Float -> Float -> Float -> Bool
goodEnough0 _ guess x = res where
    res = (<)
        (abs1 ((-) (square1 guess) x))
        0.001

goodEnough1 :: Float -> Float -> Float -> Bool
goodEnough1 _ guess x = res where
    res = (<)
        (abs1 ((-) (square1 guess) x))
        ((*) 0.001 guess)

goodEnough2 :: Float -> Float -> Float -> Bool
goodEnough2 guessPrev guess x = res where
    res1 = goodEnough1 guessPrev guess x
    diffGuess = abs1 ((-) guessPrev guess )
    res = (&&) res1
        ((<) diffGuess ((*) 0.001 guess))

sqrtIter :: (Float -> Float -> Float -> Bool) -> Float -> Float -> Float -> Float
sqrtIter goodEnough guessPrev guess x = res where
    res = if goodEnough guessPrev guess x
        then guess
        else sqrtIter goodEnough guess (improve guess x) x

check :: Float -> Float -> Float
check x sroot = res where
    expected = (*) sroot sroot
    res = abs1 ((-) expected x)


main :: IO ()
main = do
    print 2
    print $ check 2 (sqrtIter goodEnough0 0 1 2)
    print $ check 2 (sqrtIter goodEnough1 0 1 2)
    print $ check 2 (sqrtIter goodEnough2 0 1 2)

    putStrLn ""
    print 0.2
    print $ check 0.2 (sqrtIter goodEnough0 0 1 0.2)
    print $ check 0.2 (sqrtIter goodEnough1 0 1 0.2)
    print $ check 0.2 (sqrtIter goodEnough2 0 1 0.2)

    putStrLn ""
    print 0.02
    print $ check 0.02 (sqrtIter goodEnough0 0 1 0.02)
    print $ check 0.02 (sqrtIter goodEnough1 0 1 0.02)
    print $ check 0.02 (sqrtIter goodEnough2 0 1 0.02)

    putStrLn ""
    print 0.002
    print $ check 0.002 (sqrtIter goodEnough0 0 1 0.002)
    print $ check 0.002 (sqrtIter goodEnough1 0 1 0.002)
    print $ check 0.002 (sqrtIter goodEnough2 0 1 0.002)

    putStrLn ""
    print 0.0002
    print $ check 0.0002 (sqrtIter goodEnough0 0 1 0.0002)
    print $ check 0.0002 (sqrtIter goodEnough1 0 1 0.0002)
    print $ check 0.0002 (sqrtIter goodEnough2 0 1 0.0002)

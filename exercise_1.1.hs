
-- Exercise 1.1.
-- Below is a sequence of expressions. What is the result printed by the
-- interpreter in response to each expression? Assume that the sequence
-- is to be evaluated in the order in which it is presented.

-- (define a 3)
a :: Int
a = 3

-- (define b (+ a 1))
b :: Int
b = (+) a 1

-- (cond ((= a 4) 6)
--     ((= b 4) (+ 6 7 a))
--     (else 25))
cond1 :: Int
cond1 | (==) a 4 = 6
    | (==) b 4 = foldr (+) 0 [6, 7, a]
    | otherwise = 25

-- (* (cond ((> a b) a)
--     ((< a b) b)
--     (else -1))
-- (+ a 1))
cond2 :: Int
cond2 = (*) x ((+) a 1) where
    x | (>) a b = a
        | (<) a b = b
        | otherwise = -1


main :: IO ()
main = do
    print "10"
    print $ 10

    print "(+ 5 3 4)"
    print $ foldr (+) 0 [5, 3, 4]

    print "(- 9 1)"
    print $ (-) 9 1

    print "(/ 6 2)"
    print $ (/) 6 2

    print "(+ (* 2 4) (- 4 6))"
    print $ (+) ((*) 2 4) ((-) 4 6)

    print "(+ a b (* a b))"
    print $ foldr (+) 0 [a, b, ((*) a b)]

    print "(= a b)"
    print $ (==) a b

    print "(if (and (> b a) (< b (* a b)))"
    print "    b"
    print "    a)"
    print $ if (&&) ((>) b a) ((<) b ((*) a b)) then b else a

    print "(cond ((= a 4) 6)"
    print "    ((= b 4) (+ 6 7 a))"
    print "    (else 25))"
    print $ cond1
       
    print "(+ 2 (if (> b a) b a))"
    print $ (+) 2 (if (>) b a then b else a)

    print "(* (cond ((> a b) a)"
    print "    ((< a b) b)"
    print "    (else -1))"
    print "(+ a 1))"
    print $ cond2
            

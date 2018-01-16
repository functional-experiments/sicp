
-- Exercise 1.5.
-- Ben Bitdiddle has invented a test to determine whether the interpreter he
-- is faced with is using applicative-order evaluation or normal-order
-- evaluation. He defines the following two procedures:
--
-- (define (p) (p))
--
-- (define (test x y)
--   (if (= x 0)
--       0
--       y))
--
-- Then he evaluates the expression
-- 
-- (test 0 (p))
-- 
-- What behavior will Ben observe with an interpreter that uses
-- applicative-order evaluation? What behavior will he observe with
-- an interpreter that uses normal-order evaluation?

p :: Int
p = p

test :: Int -> Int -> Int
test x y
    | 0 == x = 0
    | otherwise = y


main :: IO ()
main = do
    print $ test 0 p

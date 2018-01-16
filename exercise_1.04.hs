
-- Exercise 1.4.
-- Observe that our model of evaluation allows for combinations whose
-- operators are compound expressions. Use this observation to describe
-- the behavior of the following procedure:
--
-- (define (a-plus-abs-b a b)
--   ((if (> b 0) + -) a b))

aPlusAbsB :: Int -> Int -> Int
aPlusAbsB a b = op a b
    where
        op
            | b > 0 = (+)
            | otherwise = (-)

main :: IO ()
main = do
    print $ aPlusAbsB 1 2
    print $ aPlusAbsB 1 (-2)


-- Exercise 1.17.
--
-- The exponentiation algorithms in this section are based on performing
-- exponentiation by means of repeated multiplication. In a similar way,
-- one can perform integer multiplication by means of repeated addition.
-- The following multiplication procedure (in which it is assumed that
-- our language can only add, not multiply) is analogous to the expt procedure:
--
-- (define (* a b)
--   (if (= b 0)
--       0
--       (+ a (* a (- b 1)))))

-- This algorithm takes a number of steps that is linear in b.
-- Now suppose we include, together with addition, operations double,
-- which doubles an integer, and halve, which divides an (even) integer by 2.
-- Using these, design a multiplication procedure analogous to fast-expt
-- that uses a logarithmic number of steps. 

multLinear :: Int -> Int -> Int
multLinear a b
    | 0 == b = 0
    | otherwise = a + (multLinear a (b - 1))

double :: Int -> Int
double a = a * 2

halve :: Int -> Int
halve a = div a 2

isEven :: Int -> Bool
isEven n = 0 == rem n 2

multLog :: Int -> Int -> Int -> Int
multLog p a b
    | 0 == b = p
    | isEven b = multLog p (double a) (halve b)
    | otherwise = multLog (p + a) a (b - 1)


main :: IO ()
main = do
    print $ multLinear 3 5
    print $ multLog 0 3 5

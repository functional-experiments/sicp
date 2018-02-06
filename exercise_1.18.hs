
-- Exercise 1.18.
--
-- Using the results of exercises 1.16 and 1.17, devise a procedure that
-- generates an iterative process for multiplying two integers in terms
-- of adding, doubling, and halving and uses a logarithmic number of steps.40

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
    print $ multLog 0 3 5

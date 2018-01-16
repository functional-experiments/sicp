
-- Exercise 1.2.
-- Translate the following expression into prefix form 
-- https://mitpress.mit.edu/sicp/full-text/book/ch1-Z-G-3.gif


main :: IO ()
main = do
    print $ (/)
        (foldr (+) 0 [5, 4, (-) 2 (
            (-) 3 (
                ((+) 6 (
                    (/) 4 3))))])
        (foldr (*) 1 [3, (-) 6 3, (-) 2 7])


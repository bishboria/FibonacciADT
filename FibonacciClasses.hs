module FibonacciClasses where

fibonacci :: Int -> Int
fibonacci n
    | n <= 1    = n
    | otherwise = fibonacci (n-1) + fibonacci (n-2)

main = do
    putStrLn $ show $ fibonacci 0
    putStrLn $ show $ fibonacci 1
    putStrLn $ show $ fibonacci 2
    putStrLn $ show $ fibonacci 3
    putStrLn $ show $ fibonacci 4
    putStrLn $ show $ fibonacci 5

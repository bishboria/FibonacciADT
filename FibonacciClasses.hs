module FibonacciClasses where

data Nat = Zero
         | Suc Nat
         deriving (Show)

{- Adding Nats by removing one from the
 - left and adding one on the right -}
add :: Nat -> Nat -> Nat
add (Suc m) n = add m (Suc n)
add _       n = n

{- The bad recursive way -}
fib :: Nat -> Nat
fib Zero            = Zero
fib (Suc Zero)      = (Suc Zero)
fib (Suc m@(Suc n)) = add (fib m) (fib n)

{- Iterative way to calculate fibonacci numbers -}
fib' :: Nat -> Nat
fib' = fib'Iter (Suc Zero) (Suc Zero) Zero

fib'Iter :: Nat -> Nat -> Nat -> Nat -> Nat
fib'Iter _ _ z Zero    = z
fib'Iter x y z (Suc n) = fib'Iter (add x y) x y n

{- Converting integers into Nats -}
intToFib :: Int -> Nat
intToFib = foldIntToFib Suc Zero

foldIntToFib :: (Nat -> Nat) -> Nat -> Int -> Nat
foldIntToFib _ x 0 = x
foldIntToFib f x n = foldIntToFib f (f x) (n-1)

{- Converting Nats into integers -}
fibToInt :: Nat -> Int
fibToInt = foldFibToInt (+1) 0

foldFibToInt :: (Int -> Int) -> Int -> Nat -> Int
foldFibToInt _ x Zero    = x
foldFibToInt f x (Suc s) = foldFibToInt f (f x) s

{- Displaying the results -}
fibString :: Int -> String
fibString = show . fibToInt . fib . intToFib

fib'String :: Int -> String
fib'String = show .fibToInt . fib' . intToFib

main = do
    mapM_ (putStrLn . fibString) [0..25]
    mapM_ (putStrLn . fib'String) [0..25]

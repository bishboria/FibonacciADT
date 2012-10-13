module FibonacciADT where

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
fib (Suc m@(Suc n)) = add (fib m) (fib n)
fib x               = x

{- Iterative way to calculate fibonacci numbers -}
fib' :: Nat -> Nat
fib' = fib'Iter (Suc Zero) (Suc Zero) Zero

fib'Iter :: Nat -> Nat -> Nat -> Nat -> Nat
fib'Iter _ _ z Zero    = z
fib'Iter x y z (Suc n) = fib'Iter (add x y) x y n

{- Converting integers into Nats -}
intToNat :: Int -> Nat
intToNat = foldIntToNat Suc Zero

foldIntToNat :: (Nat -> Nat) -> Nat -> Int -> Nat
foldIntToNat _ x 0 = x
foldIntToNat f x n = foldIntToNat f (f x) (n-1)

{- Converting Nats into integers -}
natToInt :: Nat -> Int
natToInt = foldNatToInt (+1) 0

foldNatToInt :: (Int -> Int) -> Int -> Nat -> Int
foldNatToInt _ x Zero    = x
foldNatToInt f x (Suc s) = foldNatToInt f (f x) s

{- Displaying the results -}
fibString :: Int -> String
fibString = show . natToInt . fib . intToNat

fib'String :: Int -> String
fib'String = show . natToInt . fib' . intToNat

main = do
    mapM_ (putStrLn . fibString) [0..25]
    mapM_ (putStrLn . fib'String) [0..25]

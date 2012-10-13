module FibonacciClasses where

data Nat = Zero
            | Suc Nat
            deriving (Show)

add :: Nat -> Nat -> Nat
add Zero n    = n
add (Suc m) n = add m (Suc n)

fibonacci :: Nat -> Nat
fibonacci Zero            = Zero
fibonacci (Suc Zero)      = (Suc Zero)
fibonacci (Suc m@(Suc n)) = add (fibonacci m) (fibonacci n)

intToFib :: Int -> Nat
intToFib n = foldIntToFib Suc Zero n

foldIntToFib :: (Nat -> Nat) -> Nat -> Int -> Nat
foldIntToFib _ x 0 = x
foldIntToFib f x n = foldIntToFib f (f x) (n-1)

fibToInt :: Nat -> Int
fibToInt n = foldFibToInt (+1) 0 n

foldFibToInt :: (Int -> Int) -> Int -> Nat -> Int
foldFibToInt _ x Zero    = x
foldFibToInt f x (Suc s) = foldFibToInt f (f x) s

fibString :: Int -> String
fibString = show . fibToInt . fibonacci . intToFib

main = do
    mapM_ (putStrLn . fibString) [0..40]

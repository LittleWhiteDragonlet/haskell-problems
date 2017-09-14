module Main where

-- Problem 1

myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2

myButLast :: [a] -> a
myButLast [] = error "No empy lists!"
myButLast [x] = error "Too few elements!"
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs

-- Problem 3

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i - 1)
elementAt _ _ = error "Index out of bounds"

-- Problem 4

myLength :: [a] -> Int
myLength = foldl (\n x -> n + 1) 0

-- Problem 5

myReverse :: [a] -> [a]
myReverse list = myReverse' list []
  where
    myReverse' [] reversed = reversed
    myReverse' (x:xs) reversed = myReverse' xs (x:reversed)

-- Problem 6

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

-- Problem 7

data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)
myFlatten (List []) = []

-- Problem 8

myCompress :: Eq a => [a] -> [a]
myCompress [] = []
myCompress (x:xs) = x : myCompress (dropWhile (== x) xs)

-- Problem 9

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:first) : pack rest
  where
    getReps [] = ([], [])
    getReps (y:ys)
            | y == x = let (f,r) = getReps ys in (y : f, r)
            | otherwise = ([], y : ys)
    (first,rest) = getReps xs

-- Problem 10

encode []     = []
encode (x:xs) = encode' 1 x xs where
    encode' n x [] = [(n, x)]
    encode' n x (y:ys)
        | x == y    = encode' (n + 1) x ys
        | otherwise = (n, x) : encode' 1 y ys

main :: IO()
main = putStrLn "Haskell problems 1-10 solutions"

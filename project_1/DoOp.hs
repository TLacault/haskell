import Data.Char (isDigit)

main :: IO ()
main = putStrLn "hello world"

myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs)
    | a == x = True
    | otherwise = myElem a xs

safeDiv :: Int -> Int -> Maybe Int
safeDiv a b
    | b == 0 = Nothing
    | otherwise = Just (a `div` b)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myIsNeg :: Int -> Bool
myIsNeg x
    | x < 0 = True
    | otherwise = False

mySub :: Int -> Int
mySub x = x - 1

safeNth :: [a] -> Int -> Maybe a
safeNth a n
    | (myLength a-1) < n = Nothing
    | myIsNeg n = Nothing
safeNth (x:xs) n
    | n > 0 = safeNth xs (mySub n)
    | otherwise = Just x

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc (Just n) = Just (1 + n)

myLookup :: Eq a => a -> [(a,b)] -> Maybe b
myLookup x [] = Nothing
myLookup x ((a,b):y)
    | a == x = Just b
    | otherwise = myLookup x y

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo x s = (<*>) (fmap x s)

readInt :: [Char] -> Maybe Int
readInt c
    | all isDigit c = Just (read c :: Int)
    | otherwise = Nothing

getLineLength :: IO Int
getLineLength = do
    x <- getLine
    return (myLength x)

printAndGetLength :: String -> IO Int
printAndGetLength xs = do
    putStrLn xs
    return (myLength xs)

boxLid :: Int -> IO ()
boxLid n = do
    putStrLn ("+" ++ replicate (n*2-2) '-' ++ "+")

boxLayer :: Int -> IO ()
boxLayer n = do
    putStr $ unlines (replicate (n-2) ("|" ++ replicate (n*2-2) ' ' ++ "|"))

boxMid :: Int -> Int -> IO ()
boxMid i n
    | i > 0 = boxLayer n
    | i > 0 = boxMid i n

testBox :: Int -> Bool
testBox n
    | n <= 2 = True
    | otherwise = False

miniBox :: Int -> IO ()
miniBox n
    | n <= 0 = putStrLn ""
    | n == 1 = putStrLn "+"
    | otherwise = putStrLn "+--+\n+--+"

printBox :: Int -> IO ()
printBox n
    | testBox n = miniBox n
printBox n = do
    boxLid n
    boxMid n n
    boxLid n
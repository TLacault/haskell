mySucc :: Int -> Int
mySucc x = x + 1

mySub :: Int -> Int
mySub x = x - 1

myIsNeg :: Int -> Bool
myIsNeg x
    | x < 0 = True
    | otherwise = False

myAbs :: Int -> Int
myAbs x
    | x < 0 = -x
    | otherwise = x

myMin :: Int -> Int -> Int
myMin a b
    | a < b = a
    | otherwise = b

myMax :: Int -> Int -> Int
myMax a b
    | a > b = a
    | otherwise = b

myTuple :: a -> b -> (a, b)
myTuple a b = (a, b)

myTruple :: a -> b -> c -> (a, b, c)
myTruple a b c = (a, b, c)

myFst :: (a, b) -> a
myFst (a, b) = a

mySnd :: (a, b) -> b
mySnd (a, b) = b

mySwap :: (a, b) -> (b, a)
mySwap (a, b) = (b, a)

myHead :: [a] -> a
myHead [] = error "list should contain at least 1 element"
myHead (a:_) = a

myTail :: [a] -> [a]
myTail [] = error "list should contain at least 1 element"
myTail (_:a) = a

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myNth :: [a] -> Int -> a
myNth a n
    | (myLength a-1) < n = error "index out of reach"
    | myIsNeg n = error "negative index"
myNth (x:xs) n
    | n > 0 = myNth xs (mySub n)
    | otherwise = x

myTake :: Int -> [a] -> [a]
myTake n a
    | myLength a < n = a
    | n <= 0 = []
myTake n (x:xs) = x:myTake (n-1) xs

myDrop :: Int -> [a] -> [a]
myDrop n a
    | myLength a < n = []
    | n <= 0 = a
myDrop n (x:xs)
    | n > 1 = myDrop (mySub n) xs
    | otherwise = xs

myAppend :: [a] -> [a] -> [a]
myAppend a b
    | myLength a <= 0 = b
    | myLength b <= 0 = a
myAppend (x:xs) y = x:myAppend xs y

myReverse :: [a] -> [a]
myReverse a = a

myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit a = myTake (myLength a - 1) a

myLast :: [a] -> [a]
myLast [] = error "empty list"
myLast a
    | myLength a == 1 = a
myLast (x:xs)
    | myLength xs > 1 = myLast xs
    | otherwise = xs
import System.IO ()
import System.Exit (exitFailure)
import System.Environment (getArgs)
import Data.Char (isDigit)
import Data.List ()
import Data.Foldable ()

data Lists = Lists{la :: [Int], lb :: [Int], ls :: String} deriving (Show, Eq)

main :: IO ()
main = do
    args <- getArgs
    checkArgs args Lists{la = [], lb = [], ls = ""}

checkArgs :: [String] -> Lists -> IO ()
checkArgs  args (Lists _ ys _)
    | myIsDigit args && digitInList args=
        myPushSwap Lists{la = map (read::String->Int) args, lb = ys, ls = ""}
    | otherwise = exitFailure

digitInList :: [String] -> Bool
digitInList [] = False
digitInList string = True

checkCharDigit :: [Char] -> Bool
checkCharDigit [] = True
checkCharDigit (x:xs)
    | isDigit x = checkCharDigit xs
    | otherwise = False

myIsDigit :: [String] -> Bool
myIsDigit [] = True
myIsDigit (x:xs)
    | checkCharDigit (x :: [Char]) = myIsDigit xs
    | otherwise = False

myPushSwap :: Lists -> IO ()
myPushSwap (Lists xs ys zs)
    | null ys && myIsSortedUp xs = putStrLn (take (length zs - 1) zs)
    | otherwise = myPushSwap (myQuickSort Lists{la = xs, lb = ys, ls = zs})

mySort :: Lists -> Lists
mySort (Lists (x:xs) ys zs)
    | minimum (x:xs) == x = pb Lists{la = x:xs, lb = ys, ls = zs}
    | otherwise = ra Lists{la = x:xs, lb = ys, ls = zs}

myQuickSort :: Lists -> Lists
myQuickSort (Lists xs ys zs)
    | null xs && myIsSortedDown ys = pushToA Lists{la = xs, lb = ys, ls = zs}
    | otherwise = mySort Lists{la = xs, lb = ys, ls = zs}

pushToA :: Lists -> Lists
pushToA (Lists xs [] zs) =  Lists{la = xs, lb = [], ls = zs}
pushToA (Lists xs ys zs) = pushToA (pa Lists{la = xs, lb = ys, ls = zs})

pa :: Lists -> Lists
pa (Lists xs [] zs) = Lists{la = xs, lb = [], ls = zs}
pa (Lists xs (y:ys) zs) = Lists{la = y:xs, lb = ys, ls = zs ++ "pa "}

pb :: Lists -> Lists
pb (Lists [] ys zs) = Lists{la = [], lb = ys, ls = zs}
pb (Lists (x:xs) ys zs) = Lists{la = xs, lb = x:ys, ls = zs ++ "pb "}

ra :: Lists -> Lists
ra (Lists [] ys zs) = Lists{la = [], lb = ys, ls = zs}
ra (Lists [x] ys zs) = Lists{la = [x], lb = ys, ls = zs}
ra (Lists (x:xs) ys zs) = Lists{la = xs ++ [x], lb = ys, ls = zs ++ "ra "}

myIsSortedUp :: [Int] -> Bool
myIsSortedUp [a]
    | length [a] <= 1 = True
    | otherwise = False
myIsSortedUp (x:y:xs)
    | x <= y = myIsSortedUp (y:xs)
    | otherwise = False
myIsSortedUp [] = True

myIsSortedDown :: [Int] -> Bool
myIsSortedDown [a]
    | length [a] <= 1 = True
    | otherwise = False
myIsSortedDown (x:y:xs)
    | x >= y = myIsSortedDown (y:xs)
    | otherwise = False
myIsSortedDown [] = True

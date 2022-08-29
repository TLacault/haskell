import System.IO ()
import System.Exit (exitWith, exitFailure)
import System.Environment (getArgs)
import Data.Char (isDigit)

data Lists = Lists{la :: [Int], lb :: [Int]} deriving (Show, Eq)

main :: IO ()
main = do
    algo <- getLine
    let rules = words algo
    args <- getArgs
    checkArgs rules args Lists{la = [], lb = []}

checkArgs :: [String] -> [String] -> Lists -> IO ()
checkArgs rules args (Lists _ ys)
    | checkFlags rules && myIsDigit args && digitInList args =
        myPushSwap rules Lists{la = map (read::String->Int) args, lb = ys}
    | otherwise = exitFailure 84

myPushSwap :: [String] -> Lists -> IO ()
myPushSwap rules lists
    | checkSorted (myFunctionPointer rules lists) = displayOK
    | otherwise = displayKO (myFunctionPointer rules lists)

myIsSorted :: [Int] -> Bool
myIsSorted [a] | length [a] <= 1 = True
myIsSorted (x:y:xs) | x <= y = myIsSorted (y:xs)
myIsSorted (x:y:_) | x > y = False
myIsSorted [] = True

checkSorted :: Lists -> Bool
checkSorted (Lists xs ys)
    | null ys && myIsSorted xs = True
    | otherwise = False

myIsString :: String -> [String] -> Bool
myIsString _ [] = False
myIsString a (x:xs)
    | a == x = True
    | otherwise = myIsString a xs

checkFlags :: [String] -> Bool
checkFlags [] = True
checkFlags (x:xs)
    | myIsString x ["sa", "sb", "sc", "pa", "pb", "ra",
        "rb", "rr", "rra", "rrb", "rrr"] = checkFlags xs
    | otherwise = False

checkCharDigit :: [Char] -> Bool
checkCharDigit [] = True
checkCharDigit (x:xs)
    | isDigit x = checkCharDigit xs
    | otherwise = False

digitInList :: [String] -> Bool
digitInList []     = False
digitInList string = True

myIsDigit :: [String] -> Bool
myIsDigit [] = True
myIsDigit (x:xs)
    | checkCharDigit (x :: [Char]) = myIsDigit xs
    | otherwise = False

functionPointerS :: [String] -> Lists -> Lists
functionPointerS ("sa":xs) lists = myFunctionPointer xs (sa lists)
functionPointerS ("sb":xs) lists = myFunctionPointer xs (sb lists)
functionPointerS ("sc":xs) lists = myFunctionPointer xs (sc 1 lists)
functionPointerS str lists       = functionPointerP str lists

functionPointerP :: [String] -> Lists -> Lists
functionPointerP ("pa":xs) lists = myFunctionPointer xs (pa lists)
functionPointerP ("pb":xs) lists = myFunctionPointer xs (pb lists)
functionPointerP str lists       = functionPointerR str lists

functionPointerR :: [String] -> Lists -> Lists
functionPointerR ("ra":xs) lists = myFunctionPointer xs (ra lists)
functionPointerR ("rb":xs) lists = myFunctionPointer xs (rb lists)
functionPointerR ("rr":xs) lists = myFunctionPointer xs (rr 1 lists)
functionPointerR ("rra":xs) lists = myFunctionPointer xs (rra lists)
functionPointerR ("rrb":xs) lists = myFunctionPointer xs (rrb lists)
functionPointerR ("rrr":xs) lists = myFunctionPointer xs (rrr 1 lists)

myFunctionPointer :: [String] -> Lists -> Lists
myFunctionPointer [] lists = lists
myFunctionPointer rules lists = functionPointerS rules lists

sa :: Lists -> Lists
sa (Lists xs ys)
    | length xs <= 1 = Lists{la = xs, lb = ys}
sa (Lists (x:y:xs) ys) = Lists{la = y:x:xs, lb = ys}

sb :: Lists -> Lists
sb (Lists xs ys)
    | length ys <= 1 = Lists{la = xs, lb = ys}
sb (Lists xs (x:y:ys)) = Lists{la = xs, lb = y:x:ys}

sc :: Int -> Lists -> Lists
sc n lists
    | n == 1 = sc 0 (sa lists)
    | otherwise = sb lists

pa :: Lists -> Lists
pa (Lists xs ys)
    | null ys = Lists{la = xs, lb = ys}
pa (Lists xs (y:ys)) = Lists{la = y:xs, lb = ys}

pb :: Lists -> Lists
pb (Lists xs ys)
    | null xs = Lists{la = xs, lb = ys}
pb (Lists (x:xs) ys) = Lists{la = xs, lb = x:ys}

ra :: Lists -> Lists
ra (Lists [] ys) = Lists{la = [], lb = ys}
ra (Lists [x] ys) = Lists{la = [x], lb = ys}
ra (Lists (x:xs) ys) = Lists{la = xs ++ [x], lb = ys}

rb :: Lists -> Lists
rb (Lists xs []) = Lists{la = xs, lb = []}
rb (Lists xs [y]) = Lists{la = xs, lb = [y]}
rb (Lists xs (y:ys)) = Lists{la = xs, lb = ys ++ [y]}

rr :: Int -> Lists -> Lists
rr n lists
    | n == 1 = rr 0 (ra lists)
    | otherwise = rb lists

rra :: Lists -> Lists
rra (Lists [] ys) = Lists{la = [], lb = ys}
rra (Lists [x] ys) = Lists{la = [x], lb = ys}
rra (Lists (x:xs) ys) = Lists{la = last xs:x:take (length xs - 1) xs, lb = ys}

rrb :: Lists -> Lists
rrb (Lists xs []) = Lists{la = xs, lb = []}
rrb (Lists xs [y]) = Lists{la = xs, lb = [y]}
rrb (Lists xs (y:ys)) = Lists{la = xs, lb = last ys:y:take (length ys - 1) ys}

rrr :: Int -> Lists -> Lists
rrr n lists
    | n == 1 = rrr 0 (rra lists)
    | otherwise = rrb lists

displayOK :: IO ()
displayOK = putStrLn "OK"

displayKO :: Lists -> IO ()
displayKO (Lists xs ys) = putStrLn ("KO: (" ++ show xs ++ "," ++ show ys ++ ")")
displayKO _ = exitFailure
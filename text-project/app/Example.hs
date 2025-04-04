module Example where

import Data.Char ( chr , ord)
-- import Distribution.Compat.Lens (set)

increas :: Int -> Int
increas _ = 3 + 1

headOrDef :: Int -> [Int] -> Int
headOrDef def list =
    if null list
        then def
        else head list

summa :: [Int] -> Int
summa list =
    if null list
        then 0
        else summa (drop 1 list) + head list

count :: [Int] -> [Int]
count list = countSub list (makeZeroList 10)

countSub :: [Int] -> [Int] ->[Int]
countSub list cou =
    if null list
        then cou
        else let x = head list
            in countSub (tail list) (concat [take x cou, [(cou !! x) + 1] ,drop (x+1) cou])

makeZeroList :: Int -> [Int]
makeZeroList num =
    if num == 0
        then []
        else 0 : makeZeroList (num - 1)

tree :: (Integer -> Integer -> Integer) -> Integer -> Integer
tree f x = 
    if x == 1
        then 1
        else f x (tree f (x-1))

checkEven :: Int -> String
checkEven i 
    | i == 0 = "The Number " ++ show i ++ " is Zero"
    | mod i 2 == 1 = "The Number " ++ show i ++ " is Odd"
    | otherwise = "The Number " ++ show i ++ " is Even"

makeNumberList :: [Int] -> String
makeNumberList [] = ""
makeNumberList (x:xs) = replicate x (chr (ord 'a' + x - 1)) ++ makeNumberList xs

oneOrTwoZeros :: [Int] -> Bool
oneOrTwoZeros l
    | l == [0] = True
    | l == [0,0] = True
    | otherwise = False

oneOrTwoZeros' :: [Int] -> Bool
oneOrTwoZeros' l = case l of
    [0]     -> True
    [0,0]   -> True
    _       -> False

atLestTwo' :: [Int] -> Bool
atLestTwo' l = case l of 
    []  -> False
    [_] -> False
    _   -> True

atLestTwo :: [Int] -> Bool
atLestTwo l = case l of
    _ : (_ : _)    -> True
    _               -> False

approxE :: Int -> Double
approxE n = sum (map (\y -> 1/product [1..y]) [1..fromIntegral n])

test :: Bool -> Bool
test True = False
test _ = True

data User = MkUser String Bool Int
    deriving (Show)

getName :: User -> String
getName (MkUser name _ _) = name

setName :: String -> User -> User
setName name (MkUser _ stu age) = MkUser name stu age

data User2 = MkUser2
    { userName       :: String
    , userAge        :: Int
    , userStudent    :: Bool
    }
    deriving (Show)

data Color 
    = Red
    | Blue
    | Orange

showColor :: Color -> String
showColor c = case c of
    Red     -> "Red"
    Blue    -> "Blue"
    Orange  -> "Orange"

data Result
    = Error String
    | Ok Int
    deriving (Show)

divide :: Int -> Int -> Result
divide _ 0 = Error "Can't divide by Zero"
divide x y = Ok (div x y)

type Health  = Int
type Attack  = Int
type Defence = Int
type Info    = (String, Int, Bool)

damage :: Defence -> Attack -> Health
damage def att = min (def - att) 0

heal :: String -> Health
heal name = case name of 
    "Large Potion"  -> 20
    "Medium Potion" -> 10
    "Small Potion"  -> 5
    _               -> 0

newtype Health2 = MkHealth2 Int 
newtype Attack2 = MkAttack2 Int
newtype Defence2 = MkDefence2 Int
newtype Info2   = MkInfo (String, Int, Bool) -- <-- Can only have one? Or not...

damage2 :: Defence2 -> Attack2 -> Health2
damage2 (MkDefence2 def) (MkAttack2 att) = MkHealth2(min (def - att) 0)

doubleList :: a -> [a]
doubleList a = [a,a]

data Chest a = MkChest 
    {someGold       :: Int
    ,someTresure    :: a
    }
    deriving (Show)
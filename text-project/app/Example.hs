module Example where

import Data.Char ( chr , ord)

increas :: Int -> Int
increas x = 3 + 1

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
module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

-- If you're not sure what this is, it's ok.
import Control.Monad (mapM)
-- import Distribution.Compat.Prelude (undefined)
-- import Distribution.Compat.Prelude (undefined)

-- A pattern is a list of things
-- Where we have either a value or a wildcard
data PatternElem a = Wildcard | Item a
  deriving (Eq, Show)

-- A pattern is a list of pattern elements
newtype Pattern a = Pattern [PatternElem a]
  deriving (Eq, Show)

-- Templates are the same as patterns
type Template a = Pattern a

-- A phrase is a list of string
type Phrase = [String]
-- type String = [Char]
newtype Rule = Rule (Pattern String, [Template String])
  deriving (Eq, Show)

type BotBrain = [Rule]

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()


--------------------------------------------------------

-- This takes a brain, and returns a function
-- Which will take a phrase as an input and calculate the result
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind b =
  fmap rulesApply (mapM makePair b)

-- A rule maps a pattern to many answers, so we choose one
-- at random, and that's our bot
makePair :: Rule -> IO (Pattern String, Template String)
{- TO BE WRITTEN -}
makePair = undefined

rulesApply :: [(Pattern String, Template String)] -> Phrase -> Phrase
{- TO BE WRITTEN -}
rulesApply = undefined

reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
reflect = undefined

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map ruleCompile

ruleCompile :: (String, [String]) -> Rule
{- TO BE WRITTEN -}
ruleCompile = undefined

--------------------------------------


-- We can make a pattern from a list of elements
-- If we choose one element that represents the wildcard
-- mkPattern '*' "Hi *!" => [Item 'H', Item 'i', Wildcard, Item '!']
mkPattern :: Eq a => a -> [a] -> Pattern a
{- TO BE WRITTEN -}
mkPattern wild list = Pattern $ map (\x -> if x == wild then Wildcard else Item x) list

stringToPattern :: String -> String -> Pattern String
stringToPattern wc = mkPattern wc . words

starPattern :: String -> Pattern String
starPattern = stringToPattern "*"

reductions :: [(Pattern String, Pattern String)]
reductions = (map . map2) (starPattern, starPattern)
  [ ( "please *", "*" ),
    ( "could you *", "*" ),
    ( "can you *", "*"),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [(Pattern String, Pattern String)] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply = undefined


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a template with the list given as the third argument
-- ex: substitute (Pattern [Item 1,Item 2,Wildcard,Item 4]) [5] => [1,2,5,4]
-- ex: substitute (Pattern [Item 1,Item 2,Wildcard,Item 4]) [5,6,7] => [1,2,5,6,7,4]
-- ex: substitute (Pattern [Item 1,Item 2,Wildcard,Item 4,Wildcard]) [-1,-2,-3] => [1,2,-1,-2,-3,4,-1,-2,-3]
substitute :: Eq a => Template a -> [a] -> [a]
{- TO BE WRITTEN -}
-- substitute = undefined
substitute (Pattern []) _ = []
substitute (Pattern (Wildcard:xs)) wild = wild ++ (substitute (Pattern xs) wild)
substitute (Pattern ((Item x):xs)) wild = x : (substitute (Pattern xs) wild)

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
-- ex: match (mkPattern '*' "frodo") "gandalf" = Nothing
-- ex: match (mkPattern 'x' "2*x+3+x") "2*7+3" = Nothing
-- ex: match (mkPattern 'x' "abcd") "abcd" = Just []
-- ex: match (mkPattern 2 [1,3..5]) [1,3..5] = Just []
-- ex: match (mkPattern 'x' "2*x+3") "2*7+3" = Just "7"
-- ex: match (mkPattern '*' "* and *") "you and me" = Just "you"
-- ex: match (Pattern [Item 1,Item 2,Wildcard,Item 4]) [1,2,5,4] => Just [5]
-- ex: match (Pattern [Item 1,Item 2,Wildcard,Item 4]) [1,2,5,6,7,4] => Just [5,6,7]
match :: Eq a => Pattern a -> [a] -> Maybe [a]
{- TO BE WRITTEN -}
-- match = undefined
match (Pattern []) _ = Nothing
match _ [] = Nothing
match (Pattern [Item p]) [x] =
    if x == p
        then Just []
        else Nothing
match (Pattern ((Item p):ps)) (x:xs) =
    if x == p
        then match (Pattern ps) xs
        else Nothing
-- match (Pattern (Wildcard:ps)) (x:xs) =

-- hej :: [Int] -> Int
-- hej [] = 0
-- hej [x] = x
-- hej (x:xs) = x + 1

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => Pattern a -> [a] -> Maybe [a]
singleWildcardMatch (Pattern (Wildcard:ps)) (x:xs) =
  case match (Pattern ps) xs of
    Nothing -> Nothing
    Just _ -> Just [x]
{- TO BE WRITTEN -}
longerWildcardMatch (Pattern (Wildcard:ps)) (x:xs) = undefined



-------------------------------------------------------
-- Applying patterns transformations
--------------------------------------------------------

-- Helper function: Matches a pattern and applies the transformation
matchAndTransform :: Eq a => ([a] -> [a]) -> Pattern a -> [a] -> Maybe [a]
matchAndTransform transform pat = (mmap transform) . (match pat)

-- Applying a single pattern
transformationApply :: Eq a => ([a] -> [a]) -> [a] -> (Pattern a, Template a) -> Maybe [a]
{- TO BE WRITTEN -}
transformationApply = undefined

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => ([a] -> [a]) -> [(Pattern a, Template a)] -> [a] -> Maybe [a]
{- TO BE WRITTEN -}
transformationsApply = undefined

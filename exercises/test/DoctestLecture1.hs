module DoctestLecture1 (main) where

import Test.DocTest (doctest)


main :: IO ()
main = doctest
    [ "src/Lecture1.hs"
    ]

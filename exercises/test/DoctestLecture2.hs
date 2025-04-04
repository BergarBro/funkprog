module DoctestLecture2 (main) where

import Test.DocTest (doctest)


main :: IO ()
main = doctest
    [ "src/Lecture2.hs"
    ]

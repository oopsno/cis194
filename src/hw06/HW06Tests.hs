module Main where

import Test.HUnit
import CIS194.HUnit
import HW06          hiding ( main )

tests :: Test
tests =  test [ "exercise 5" ~: test [ take 5 (streamToList (sIterate ('x' :) "o")) ~=? ["o", "xo", "xxo", "xxxo", "xxxxo"]
                                     , take 5 (streamToList (sInterleave (sRepeat 0) (sRepeat 1))) ~=? [0, 1, 0, 1, 0]
                                     , sTake 3 (sRepeat 0) ~=? [0, 0, 0]
                                     ]
              ]

main :: IO ()
main = runner tests

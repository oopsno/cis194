module Main where

import Prelude    ( IO, (>>), return )

import Test.HUnit
import CIS194.HUnit
import HW03

tests :: Test
tests =  test [ "exercise 1" ~: 0 ~=? (empty "A")
              , "exercise 2" ~: test [ 5 ~=? evalE empty (Val 5)
                                     , 0 ~=? evalE empty (Op (Val 1) Eql (Val 2))
                                     ]
              , "exercise 3" ~: desugar (Incr "A") ~=? DAssign "A" (Op (Var "A") Plus (Val 1))
              , "exercise 4" ~: test [ 10 ~=? evalSimple empty (DAssign "A" (Val 10)) "A"
                                     , 24 ~=?  run (extend empty "In" 4) factorial "Out"
                                     ]
              ]

main :: IO ()
main = runner tests

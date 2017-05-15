module Main where

import Test.HUnit
import CIS194.HUnit
import HW04

tests :: Test
tests =  test [ "exercise 1" ~: x ~=? P [0, 1]
              , "exercise 2" ~: test [ True ~=? P [1, 2, 3] == P [1, 2, 3]
                                     , True ~=? P [1, 2]    /= P [1, 2, 3]
                                     ]
              , "exercise 3" ~: test [ "2x^3 + 1"  ~=? show (P [1,  0, 0, 2])
                                     , "2x^2 + -x" ~=? show (P [0, -1, 2])
                                     ]
              , "exercise 4" ~: test [ P [5, 0, 1] + P [1, 1, 2] ~=? P [6, 1, 3]
                                     , P [1, 0, 1] + P [1, 1]    ~=? P [2, 1, 1]
                                     ]
              , "exercise 5" ~: P [1, 1, 1] * P [2, 2] ~=? P [2, 4, 4, 2]
              , "exercise 6" ~: negate (P [1, 2, 3]) ~=? P [-1, -2, -3]
              , "exercise 7" ~: test [ 4 ~=? applyP (x^2 + 2*x + 1) 1
                                     , 9 ~=? applyP (x^2 + 2*x + 1) 2
                                     ]
              , "exercise 9" ~: deriv (x^2 + 3*x + 5) ~=? 2*x + 3
              ]

main :: IO ()
main = runner tests

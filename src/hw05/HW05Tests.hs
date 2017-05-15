module Main where

import Test.HUnit
import CIS194.HUnit
import Data.Map.Strict ( Map, fromList  )
import Parser
import HW05 hiding     ( main )

ts :: [Transaction]
ts = [ Transaction { from = "Haskell Curry"
                   , to = "Simon Peyton Jones"
                   , amount = 10
                   , tid = "534a8de8-5a7e-4285-9801-8585734ed3dc"
                   } ]

mp :: Map String Integer
mp = fromList [ ("Haskell Curry", -10)
              , ("Simon Peyton Jones", 10)
              ]

tests :: Test
tests = test [ "exercise 5" ~: mp ~=? getFlow ts ]

main :: IO ()
main = runner tests

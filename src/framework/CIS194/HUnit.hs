module CIS194.HUnit ( runner ) where

import System.Exit         ( ExitCode(..), exitWith )
import Test.HUnit          ( Test, Counts(..), runTestTT )

summary :: Counts -> ExitCode
summary c = 
  if cases c == tried c && errors c == 0 && failures c == 0
    then ExitSuccess
    else ExitFailure $ 1 + errors c + failures c

runner :: Test -> IO ()
runner test = runTestTT test >>= exitWith . summary
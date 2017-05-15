module Main where

import System.Exit ( exitSuccess, exitFailure )

import HW02Tests   ( allTests )
import Testing     ( runTests )

main :: IO ()
main = do
  let failures = runTests allTests in
    if length failures == 0
        then exitSuccess
        else mapM_ print failures >> exitFailure

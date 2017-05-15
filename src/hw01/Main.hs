module Main where

import HW01Tests        ( allTests )
import CIS194.Testing   ( runner )

main :: IO ()
main = runner allTests

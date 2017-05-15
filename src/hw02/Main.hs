module Main where

import HW02Tests        ( allTests )
import CIS194.Testing   ( runner )

main :: IO ()
main = runner allTests

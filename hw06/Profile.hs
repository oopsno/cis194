module Main where 

import HW06 ( fastFib )

main :: IO ()
main = print $ fastFib 100000000 -- 100M

module Main ( main ) where

import Parser
import System.Environment   ( getArgs )

import qualified HW05
import qualified Paths_cis194 as P

doEverything :: [FilePath] -> IO String
doEverything (a:b:c:x:y:z:_) = HW05.doEverything a b c x y z

main :: IO ()
main = do
  args <- getArgs
  defaultPathes <- mapM P.getDataFileName [ "dog-original.jpg"
                                          , "dog.jpg"
                                          , "transactions.json"
                                          , "victims.json"
                                          , "new-ids.json"
                                          , "new-transactions.json" ]
  let files = if length args >= 6 then args else defaultPathes
  doEverything files >>= putStrLn
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.Bits
import Data.List
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret raw sec = BS.pack . filter (/=0) <$> liftM2 (BS.zipWith xor) (BS.readFile raw) (BS.readFile sec)

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = 
 BS.pack . BS.zipWith xor (BS.cycle key) <$> BS.readFile (path ++ ".enc") >>= BS.writeFile path 

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = decode <$> BS.readFile path

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs v t = do
  tids  <- parseFile v :: IO (Maybe [TId])
  trans <- parseFile t :: IO (Maybe [Transaction])
  return $ liftM2 check tids trans
  where check :: [TId] -> [Transaction] -> [Transaction]
        check xs = filter (\y -> tid y `notElem` xs)

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow ts = foldl up (Map.fromList (zip (map from ts ++ map to ts) (repeat 0))) ts
  where up m t = let n  = amount t
                     m' = Map.adjust (+ n) (to t) m
                 in  Map.adjust (flip (-) n) (from t) m'

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . head . sortOn snd . Map.toList 

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m ts = 
  let payers = descSort $ Map.toList $ Map.filter (> 0) m
      payees = descSort $ Map.toList $ Map.filter (< 0) m
  in zipWith3 payback payers payees ts
  where descSort = reverse . sortOn (abs . snd)
        payback (x, xa) (y, ya) = Transaction x y (minimum [xa, abs ya])
  
-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON p = BS.writeFile p . encode

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim


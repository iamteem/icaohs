-- # {-# LANGUAGE OverloadedStrings, RebindableSyntax #-}
module Lib
    where

import qualified Data.Text as T
import System.Environment
import qualified Data.HashTable.IO as H
import qualified Control.Monad as M (forM_, mapM)
import Data.Char (toUpper)
import Data.List (intercalate)

icaoMapList = [('A', "Alfa"),
               ('B', "Bravo"),
               ('C', "Charlie"),
               ('D', "Delta"),
               ('E', "Echo"),
               ('F', "Foxtrot"),
               ('G', "Golf"),
               ('H', "Hotel"),
               ('I', "India"),
               ('J', "Juliett"),
               ('K', "Kilo"),
               ('L', "Lima"),
               ('M', "Mike"),
               ('N', "November"),
               ('O', "Oscar"),
               ('P', "Papa"),
               ('Q', "Quebec"),
               ('R', "Romeo"),
               ('S', "Sierra"),
               ('T', "Tango"),
               ('U', "Uniform"),
               ('V', "Victor"),
               ('W', "Whiskey"),
               ('X', "Xray"),
               ('Y', "Yankee"),
               ('Z', "Zulu")]


type HashTable k v = H.BasicHashTable k v

htICAO :: IO (HashTable Char String)
htICAO = H.fromList icaoMapList

translateC :: Char -> IO (String)
translateC c = do
  h <- htICAO
  m <- H.lookup h $ toUpper c
  return $ case m of
    Nothing -> [c]
    Just s -> s

translate :: String -> IO [String]
translate s = M.mapM translateC s

run :: IO ()
run = do
  args <- getArgs
  M.forM_ args $ \word -> do
    translated <- translate word
    putStrLn $ intercalate " " translated
  return ()

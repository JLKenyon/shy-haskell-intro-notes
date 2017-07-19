module Main where

import Control.Monad
import Text.Parsec
import Text.Parsec.Char

main :: IO ()
main = all_main

all_main :: IO ()
all_main = do
  putStrLn "================== Main 1 =================="
  main1
  putStrLn "================== Main 2 =================="
  main2
  putStrLn "================== Main 3 =================="
  main3
  putStrLn "================== Main 4 =================="
  main4


-------------------------------------------------------------------------------
-- Main 1

myParser1 = do
  char 'a'
  space
  char 'b'
  eof

myValidInput1 = "a b"
myInvalidInput1 = "no"

main1 :: IO ()
main1 = do
  print $ parse myParser1 "Hardcoded input" myValidInput1
  print $ parse myParser1 "Hardcoded input" myInvalidInput1
  
-------------------------------------------------------------------------------
-- Main 2

myParser2 = do
  many (char 'a')
  optional (space)
  many (char 'b')
  eof

myValidInput2_1 = "aaaa bb"
myValidInput2_2 = "ab"
myValidInput2_3 = "bbbbb"
myInvalidInput2 = "no"

main2 :: IO ()
main2 = do
  print $ parse myParser2 "Hardcoded input" myValidInput2_1
  print $ parse myParser2 "Hardcoded input" myValidInput2_2
  print $ parse myParser2 "Hardcoded input" myValidInput2_3
  print $ parse myParser2 "Hardcoded input" myInvalidInput2
 
-------------------------------------------------------------------------------
-- Main 3

myParser3 = do
  string "var"
  skipMany1 space
  var_name <- many1 alphaNum
  char ';'
  eof
  return var_name

myValidInput3 = "var bob;"
myInvalidInput3 = "nope"

main3 :: IO ()
main3 = do
  print $ parse myParser3 "Hardcoded input" myValidInput3
  print $ parse myParser3 "Hardcoded input" myInvalidInput3
 
-------------------------------------------------------------------------------
-- Main 4

myVariableParser4 = do
  spaces
  string "var"
  skipMany1 space
  var_name <- many1 alphaNum
  char ';'
  return var_name

myParser4 = do
  variables <- many myVariableParser4
  eof
  return variables

myValidInput4 = "var alice; var bob; var charlie;"
myInvalidInput4 = "nope"

main4 :: IO ()
main4 = do
  print $ parse myParser4 "Hardcoded input" myValidInput4
  print $ parse myParser4 "Hardcoded input" myInvalidInput4
 

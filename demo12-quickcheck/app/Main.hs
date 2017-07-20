module Main where

import Test.QuickCheck


test1_val_less_than_twenty :: Int -> Bool
test1_val_less_than_twenty x = x < 20


main :: IO ()
main = do
  putStrLn "--------------------------------------"
  verboseCheck test1_val_less_than_twenty



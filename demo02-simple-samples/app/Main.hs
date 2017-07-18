module Main where


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
main1 :: IO ()
main1 = putStrLn "Hello world"


-------------------------------------------------------------------------------
-- Main 2
main2 :: IO ()
main2 = do
  putStrLn "Hello"
  putStrLn "world"


-------------------------------------------------------------------------------
-- Main 3
size_of_world = 4

main3 :: IO()
main3 = do
  putStrLn "Hello"
  if size_of_world > 5
    then putStrLn "Big"
    else putStrLn "Small"
  putStrLn "World"


-------------------------------------------------------------------------------
-- Main 4
main4 :: IO()
main4 = do
  putStrLn "Please type something and press enter"
  user_input <- getLine
  putStrLn ("You typed: \"" ++ user_input ++ "\"")



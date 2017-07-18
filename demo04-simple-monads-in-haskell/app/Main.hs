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

say_hello :: IO ()
say_hello = putStrLn "Hello "

say_monoid :: IO ()
say_monoid = putStrLn "Monoid "

say_world :: IO ()
say_world = putStrLn "World "


-------------------------------------------------------------------------------
-- Main 1

main1 :: IO ()
main1 = do
  putStrLn "The IO Monad is like a \"ThingThatCanBeDone\" from the C++ example"
  say_hello
  say_monoid
  say_world

-------------------------------------------------------------------------------
-- Main 2

say_hello_monoid_world = say_hello >> say_monoid >> say_world

main2 :: IO ()
main2 = do
  putStrLn "And we can add them, but we will use the >> operator instead of +"
  say_hello_monoid_world

-------------------------------------------------------------------------------
-- Main 3

main3 :: IO ()
main3 = putStrLn "In fact, the \"do\" block in Haskell is just syntactic sugar, for the >> (and >>= coming soon) operators!" >> say_hello >> say_monoid >> say_world


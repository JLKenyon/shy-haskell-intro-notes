module Main where

import Control.Monad

main :: IO ()
main = do
  putStrLn "================== Main 1 =================="
  main1
  putStrLn "================== Main 2 =================="
  main2
  putStrLn "================== Main 3 =================="
  main3
  putStrLn "================== Main 4 =================="
  main4
  putStrLn "================== Main 5 =================="
  main5

main1 :: IO ()
main1 = do
  putStrLn "Last time we learned that Monads are basically composable statements"
  putStrLn "Now lets just do some things with these \"statements\""
  putStrLn "Specifically, the IO monad, which is basically \"Do these in order\" Monad"
  putStrLn "(Foreshadowing!)"


-- ++ is string concatenation, show is toString

main2 :: IO ()
main2 = do
  putStrLn "Lets make monadic statements that look like normal imperative assignment"
  putStrLn "We have the syntactic sugar of <- to assign a variable like name"
  x <- return 5
  putStrLn ("I see a value: " ++ (show x))
  putStrLn "I can do simple math on it"
  y <- return (2 * x)
  putStrLn ("Double the original value is " ++ (show y))


main3 :: IO ()
main3 = do
  putStrLn "Now we can "
  putStrLn "Do something N times"
  replicateM_ 5 (putStrLn "Hello")

-- \ x ->  is a lambda function with parameter x

main4 :: IO ()
main4 = do
  putStrLn "A for loop facimile"
  forM_ [1,2,3,4,5] (\x -> putStrLn ("Value: " ++ (show x)))
  putStrLn "And the same thing but with the parameters flipped"
  mapM_ (\x -> putStrLn ("Value: " ++ (show x))) [1,2,3,4,5]


main5 :: IO ()
main5 = do
  putStrLn "We can also deal with other kinds of monads"
  putStrLn "For example, Lists are Monads!"
  putStrLn "Not just Monoids as we saw before, but Monads, where the >> operator means Cartesian-Cross"
  putStrLn $ show $ do
    x <- [1,2,3]
    y <- ["a", "b", "c"]
    return (x,y)
  putStrLn "There is also the Maybe monad, which can Just be a value, or Nothing."
  putStrLn "This also has a linear sequence, until it hits a Nothing value"
  putStrLn $ show $ do
    Just 5
    Just 4
    Just 3
  putStrLn $ show $ do
    Just 5
    Nothing
    Just 3


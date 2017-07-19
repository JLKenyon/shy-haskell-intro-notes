module Main where

-- For this example, lets make a monad which records a logged history of sequential (non IO) actions
-- 
-- Lets start with the goal, we want to write a "program" that looks like this
-- transaction_log = do
--   x <- my_log "Initial value" 5
--   y <- my_log "Second value" 6
--   s <- my_log "Sum" $ x + y
--   p <- my_log "Product" $ x * y
--   my_log "Difference between product and sum" (p - s)


-- In order to fulfill the Monad contract we need a data structure, and to implement
--   the Monad, Functor and Applicative type classes

-- Each LogEntry has a concrete value to represent the history, and a variable type
-- which represents the subsequent monad values.  Think of this as the link that 
-- allows Monads to behave like a list.
-- In this case, the history will be represented as a list of entries, each entry
-- will be a tuple of comment string and value string.
data LogEntry a = LogEntry {
  history :: [(String, String)],
  next :: a
} deriving(Show)

-- For now, ignore the code from here...
instance Functor LogEntry where
  fmap f (LogEntry h n) = LogEntry h (f n)

instance Applicative LogEntry where
  (LogEntry h1 fn) <*> (LogEntry h2 val) = LogEntry (h1 ++ h2) (fn val)
  pure x = LogEntry [] x
-- ... to here

-- The two basic requirements of a Monad are
-- 1) Handle the sequential relationship between two instances, in our case, update the history
-- 2) Perform the associated work
instance Monad LogEntry where
  return x = LogEntry [] x
  (LogEntry h1 val) >>= fn = LogEntry (h1 ++ h2) new_val
  --                                  ^^^^^^^^^^ update the history
    where
      (LogEntry h2 new_val) = fn val
      --                      ^^^^^^ Do the associated work

-- Instead of using the LogEntry type constructor, lets make a pair of wrapper functions
-- The first will be used to perform a computation and log the result
my_log :: Show a => String -> a -> LogEntry a
my_log s v = LogEntry [(s, show v)] v

-- The second will dump the history to a string
dump_logs :: LogEntry a -> String
dump_logs (LogEntry h _) = unlines $ map join_logs h
  where
    join_logs :: (String, String) -> String
    join_logs (x,y) = x ++ " : " ++ y

-- Now lets make an example Monadic "program" that we can run
transaction_log = do
  x <- my_log "Initial value" 5
  y <- my_log "Second value" 6
  s <- my_log "Sum" $ x + y
  p <- my_log "Product" $ x * y
  if p > s 
    then my_log "Difference between product and sum" $ p - s
    else my_log "Difference between sum and product" $ s - p

-- A simple main which runs the above "program" and then prints the logs
main :: IO ()
main = putStrLn $ dump_logs $ transaction_log


set -e
(
  echo demo01-hello-world
  cd demo01-hello-world
  stack build
)
(
  echo demo02-simple-samples
  cd demo02-simple-samples
  stack build
)
(
  echo demo03-monads-are-monoids
  cd demo03-monads-are-monoids
  make
)
(
  echo demo04-simple-monads-in-haskell
  cd demo04-simple-monads-in-haskell
  stack build
)
(
  echo demo05-statements-vs-expressions
  cd demo05-statements-vs-expressions
  make
)
(
  echo demo06-complete-the-monad
  cd demo06-complete-the-monad
  stack build
)
(
  echo demo07-logging-monad
  cd demo07-logging-monad
  stack build
)
(
  echo demo08-coroutines-monad-style
  cd demo08-coroutines-monad-style
  stack build
)
(
  echo demo09-monadic-parser-combinators
  cd demo09-monadic-parser-combinators
  stack build
)

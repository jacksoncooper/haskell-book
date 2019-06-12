-- Page 856

module Chapter22.Ask where

newtype Reader r a =
  Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id
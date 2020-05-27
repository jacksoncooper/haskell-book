-- Page 856

newtype Reader r a =
  Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id
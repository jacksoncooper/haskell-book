-- Page 904

import Text.Trifecta
import Text.Parser.Combinators (eof)

-- Parsers can be thought of as functioning like the State monad, taking a piece
-- of serialized input and producing a value along with the unprocessed input.

-- (>>) :: Monad m => m a -> m b -> m b
-- (>>) State s a -> State s b -> State s b
-- (>>) s -> (a, s) ->
--      s -> (b, s) ->
--      s -> (b, s)

stop = unexpected "stop"
one = char '1'
one' = one >> stop
oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse parser =
    print $ parseString parser mempty "123"

printNewline :: String -> IO ()
printNewline string =
  putStrLn ('\n' : string)

-- 1.

oneExhaust = one >> eof
oneTwoExhaust = oneTwo >> eof

-- 2.

-- oneTwoThree =

-- 3.

string' :: CharParsing m => String -> m String
string' = traverse char

main = do
  printNewline "stop:"
  testParse stop

  printNewline "one:"
  testParse one

  printNewline "one':"
  testParse one'

  printNewline "oneTwo:"
  testParse oneTwo

  printNewline "oneTwo'"
  testParse oneTwo'

  -- 1.

  printNewline "oneExhaust:"
  print $ parseString oneExhaust mempty "123"

  printNewline "oneTwoExhaust:"
  print $ parseString oneTwoExhaust mempty "123"

  -- 2.

  -- printNewline "oneTwoThree:"
  -- print $ parseString oneTwoThree mempty "1"

-- Page 912

import Text.Trifecta

onlyInteger :: Parser Integer
onlyInteger =
  integer >>=
    \someInteger ->
      eof >>
        return someInteger

main :: IO ()
main = do
  putStrLn . show $ parseString onlyInteger mempty "123"
  putStrLn . show $ parseString onlyInteger mempty "123abc"

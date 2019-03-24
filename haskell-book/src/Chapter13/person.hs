-- Chapter Exercises, Page 530

type Name = String
type Age = Integer

data Person = Person Name Age
  deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

makePerson :: Name -> Age -> Either PersonInvalid Person
makePerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == ""            = Left NameEmpty
  | age <= 0              = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $
      "Name was '" ++ name ++ "' and age was: '" ++ show age ++ "'."

gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter the name of the person: "
  name <- getLine

  putStr "Enter the age of the person: "
  age <- getLine

  case makePerson name (read age) of
    Right person@(Person _ _) ->
      putStrLn $ "Yay! Successfully got a person: " ++ (show person)
    Left NameEmpty ->
      putStrLn "Error: The name of the person was empty."
    Left AgeTooLow ->
      putStrLn "Error: The age of the person was not greater than zero."
    Left (PersonInvalidUnknown _) ->
      putStrLn "Error: I don't know what you did but something horrible happened."
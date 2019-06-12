-- Page 857

module Chapter22.Person where

import Control.Applicative (liftA2)

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

bird :: Person
bird =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris =
  Person (HumanName "Chris Allen")
         (DogName "Papu")
         (Address "Austin")

getDog :: Person -> Dog
getDog person =
  Dog (dogName person) (address person)

getDog' :: Person -> Dog
getDog' =
  Dog <$> dogName <*> address

getDog'' :: Person -> Dog
getDog'' =
  liftA2 Dog dogName address

getDog''' :: Person -> Dog
getDog''' = do
  name <- dogName
  addy <- address
  return $ Dog name addy
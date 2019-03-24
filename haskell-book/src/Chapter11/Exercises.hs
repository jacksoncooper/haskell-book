module Chapter11.Exercises where

import Data.Int

-- Exercises: Dog Types p.396

data PugType = PugData

data HuskyType a = HuskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a = Husky a | Mastiff a
  deriving (Eq, Show)

-- 1. Doggies is a type constructor.
-- 2. The kind of Doggies is * -> *.
-- 3. The kind of Doggies String is *.
-- 4. The type of Husky 10 is Num a => Doggies a.
-- 5. The type of Husky (10 :: Integer) is Doggies Integer.
-- 6. The type of Mastiff "Scooby Doo" is Doggies String.
-- 7. DogueDeBordeaux is the name of both a type constructor and data
--    constructor.
-- 8. The type of DogueDeBordeaux is DogueDeBordeaux :: doge -> DogueDeBordeaux
--    doge.
-- 9. The type of DogueDeBordeaux "Doggie" is DogueDeBordeaux String.


-- Exercises: Vehicles p.399

type Size = Integer

data Price = Price Integer
  deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata
  deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size
  deriving (Eq, Show)

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir

-- 1. The type of myCar is Vehicle.

-- 2.

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

-- 3.

getManufacturer :: Vehicle -> Maybe Manufacturer
getManufacturer (Car manufacturer _)  = Just manufacturer
getManufacturer _ = Nothing

-- 4. The function will return Nothing :: Maybe.

-- 5. Done.


-- Exercises: Cardinality p.404

-- 1. The cardinality of PugType is 1.

-- 2. The cardinality of Airline is 3.

-- 3. maxBound :: Int16  =  32767
--    minBound :: Int 16 = -32768
--    The cardinality of Int16 is 32767 + 1 + 32768 = 65536.

-- 4. The cardinality of Int is 18446744073709551616.
--    The cardinality of Integer is, oh. It's unbounded. So infinite.

-- 5. Int8 has a cardinality of 256, i.e., 2 ^ 8.


-- Exercises: For Example p.405

data Example = MakeExample
  deriving Show

-- 1. MakeExample :: Example. :t can only be used on data constructors, and
--    Example is a type constructor.

-- 2.
-- :info Example
-- data Example = MakeExample  -- Defined at exercises.hs:83:1
-- instance [safe] Show Example -- Defined at exercises.hs:84:12

-- 3.

data OtherExample = MakeOtherExample Int
  deriving Show

-- MakeOtherExample :: Int -> OtherExample


-- Playground p.408

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- tooMany 40 will not resolve because 40 :: Num a => a and is ambiguous. 40
-- may be an Int, but could also be an Integer.

newtype Goats = Goats Int
  deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43

-- ^ Different behavior than the instance of TooMany for Int's, even though
--   the underlying type of Goats is an Int. Too many Int's is more than 42,
--   too many Goats is more than 43. This is a key difference between newtype
--   and type synonyms.

-- tooMany (Goats 40) will resolve, because Haskell is able to constrain the
-- polymorphic value passed into the Goats constructor.

type Goats' = Int

-- This doesn't compile. Haskell won't allow instance declarations that
-- implement a type class for a type synonym.

-- instance TooMany Goats' where
--   tooMany n = n > 43


-- Exercises: Logic Goats p.410

-- 1.

newtype IntString = IntString (Int, String)
  deriving Show

-- ^ Is this not a product type? "A newtype cannot be a product type, sum type,
--   or contain nullary constructors."

instance TooMany IntString where
  tooMany (IntString (int, string)) = int + length string > 42

-- instance TooMany (Int, String) where
--   tooMany (int, string) = int + length string > 42

-- ^ Requires language pragma FlexibleInstances, because (Int, String) is not
--   a distinct type variable. It's a composition of Int and String.

-- 2.

newtype IntInt = IntInt (Int, Int)
  deriving Show

instance TooMany IntInt where
  tooMany (IntInt (intOne, intTwo)) = intOne + intTwo > 42

-- 3.

newtype MonoProduct a = MonoProduct (a, a)
  deriving Show

-- ^ Adapted from Stack Overflow answer by Daniel Wagner:
--   https://bit.ly/2ShHEhZ.

instance (Num a, TooMany a) => TooMany (MonoProduct a) where
    tooMany (MonoProduct (x, y)) = tooMany $ x + y


-- Exercises: Pity the Bool p.412

-- 1.

data BigSmall = Big Bool | Small Bool
  deriving (Eq, Show)

-- |BigSmall| = |Bool| + |Bool|
--            = 42 + 2
--            = 4

-- 2.

data NumberOrBool = Numba Int8 | BoolyBool Bool
  deriving (Eq, Show)

-- |NumberOrBool| = |Int8| + |Bool|
--                = 256 + 2
--                = 258


-- Exercises: How Does Your Garden Grow? p.420

data FlowerType = Gardenia | Daisy | Rose | Lilac
  deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType
  deriving Show

-- 1. The 'sum of products' normal form of Garden is:

data Garden' =
    Gardenia' Gardener
  | Daisy' Gardener
  | Rose' Gardener
  | Liliac' Gardener
  deriving Show


-- Playground p.422

data Product a b = Product a b
  deriving (Eq, Show)

data Sum a b = First a | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { first :: a
                , second :: b }
                deriving (Eq, Show)

type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo = CowInfo Name Age
  deriving (Eq, Show)

data PigInfo = PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)


-- Exercise: Programmers p.430

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang}
             deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = operatingSystem, lang = language}
  | operatingSystem <- allOperatingSystems, language <- allLanguages]

allProgrammers' :: [Programmer]
allProgrammers' = go allOperatingSystems allLanguages
  where
    go [] _     = []
    go (x:xs) ys = map (Programmer x) ys ++ go xs ys

-- length allProgrammers == length allOperatingSystems * length allLanguages


-- Exponentiation in what order? p.439

-- |Quantum| = 3
-- |Bool| = 2
-- |Quantum -> Bool| = 2 ^ 3 = 8

data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)
  
-- (T, T, T)
convert1 :: Quantum -> Bool
convert1 Yes  = True
convert1 No   = True
convert1 Both = True
  
-- (T, T, F)
convert2 :: Quantum -> Bool
convert2 Yes  = True
convert2 No   = False
convert2 Both = True

-- (T, F, T)
-- ...

-- (T, F, F)
-- (F, T, T)
-- (F, T, F)
-- (F, F, T)
-- (F, F, F)


-- Exercises: The Quad p.439

data Quad = One | Two | Three | Four
  deriving (Eq, Show)

-- 1. eQuad :: Either Quad Quad

--    |Quad|  = 4
--    |eQuad| = |Left {One, Two, Three, Four}| + |Right {One, Two, Three, Four}|
--    |eQuad| = 4 + 4 = 8

-- 2. prodQuad :: (Quad, Quad)

--    |prodQuad| = 4 * 4 = 16
--    |prodQuad| = |({One, Two, Three, Four}, {One, Two, Three, Four})|

-- 3. funcQuad :: Quad -> Quad

--    |funcQuad| = 4 ^ 4 = 256
--    |funcQuad| = |({One, Two, Three, Four},
--                   {One, Two, Three, Four},
--                   {One, Two, Three, Four},
--                   {One, Two, Three, Four})|

-- 4. prodTBool :: (Bool, Bool, Bool)

--    |prodTBool| = 2 * 2 * 2 = 8
--    |prodTBool| = |({True, False}, {True, False}, {True, False})|

-- 5. gTwo :: Bool -> Bool -> Bool
--         :: Bool -> (Bool -> Bool)

--    |Bool -> Bool| = |({True, False}, {True, False})|
--                   = 2 ^ 2 = 4

--    |Bool -> (Bool -> Bool)| = 4 ^ 2 = 16

--    |gTwo| = 16

-- 6. fTwo :: Bool -> Quad -> Quad

--    |fTwo| = (4 ^ 4) ^ 2 = 65536
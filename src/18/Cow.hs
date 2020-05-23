-- Page 750

-- An example of the Maybe monad:

data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty string = Just string

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck cow =
  if name cow == "Bess" && weight cow > 499
  then Nothing
  else Just cow

-- Bad good Cow constructors, redundant outer structure:

makeSphericalCow :: String -> Int -> Int -> Maybe (Maybe Cow)
makeSphericalCow name age weight =
  fmap weightCheck $ Cow <$> noEmpty name <*> noNegative age <*> pure weight

makeSphericalCow' :: String -> Int -> Int -> Maybe (Maybe Cow)
makeSphericalCow' name age weight =
   fmap (weightCheck . uncurryThree Cow) $ ( , , ) <$> noEmpty name <*> noNegative age <*> pure weight

uncurryThree :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurryThree f (a, b, c) = f a b c

-- Good Cow constructors:

makeSphericalCow'' :: String -> Int -> Int -> Maybe Cow
makeSphericalCow'' name age weight = do
  name' <- noEmpty name
  age' <- noNegative age
  weight' <- noNegative weight
  weightCheck $ Cow name' age' weight'

makeSphericalCow''' :: String -> Int -> Int -> Maybe Cow
makeSphericalCow''' name age weight =
  collapseMaybe $ fmap weightCheck $ Cow <$> noEmpty name <*> noNegative age <*> pure weight

-- ^ Uses specified bind (>>=) function 'collapseMaybe'.

collapseMaybe :: Maybe (Maybe a) -> Maybe a
collapseMaybe (Just (Just a)) = Just a
collapseMaybe _ = Nothing
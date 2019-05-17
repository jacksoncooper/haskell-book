-- Page 756

module Chapter18.Software where

-- An example of the Either monad:

type Founded = Int
type Coders = Int

data SoftwareShop =
  Shop {
      founded :: Founded
    , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0     = Left $ NegativeYears n
  | n > 500   = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0     = Left $ NegativeCoders n
  | n > 5000  = Left $ TooManyCoders n
  | otherwise = Right n

makeSoftware :: Int -> Int -> Either FoundedError SoftwareShop
makeSoftware years coders = do
  founded     <- validateFounded years
  programmers <- validateCoders coders

  if programmers > div founded 10
  then Left $ TooManyCodersForYears founded programmers
  else Right $ Shop founded programmers

makeSoftware' :: Int -> Int -> Either FoundedError SoftwareShop
makeSoftware' years coders = do
  validateFounded years >>=

    \founded ->
      validateCoders coders >>=

        \programmers ->
          if programmers > div founded 10
          then Left $ TooManyCodersForYears founded programmers
          else Right $ Shop founded programmers
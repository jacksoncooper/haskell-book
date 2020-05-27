-- Page 720

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList' a = ZipList' [a]
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ repeat a
  (<*>) (ZipList' fs) (ZipList' xs) = ZipList' $ zipWith ($) fs xs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    listOfA <- arbitrary
    return $ ZipList' listOfA

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = let (ZipList' l) = xs
            in take 3000 l
      ys' = let (ZipList' l) = ys
            in take 3000 l

-- Testing.

myZipList :: ZipList' (Integer, Integer, Integer)
myZipList = undefined

testZipListApplicative :: IO ()
testZipListApplicative = quickBatch $ applicative myZipList
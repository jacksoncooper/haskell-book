-- Chapter Exercises, Page 572

import Test.QuickCheck

data Fool = Fulse | Frue
  deriving (Eq, Show)

-- 1.

foolGeneratorEqual :: Gen Fool
foolGeneratorEqual = elements [Fulse, Frue]

-- 2.

foolGeneratorBiased :: Gen Fool
foolGeneratorBiased = do
  frequency [(1, return Frue), (2, return Fulse)]
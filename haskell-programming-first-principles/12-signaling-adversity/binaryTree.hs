-- Chapter Exercises, p.486

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- 1.

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x =
  case f x of
    Just (x', y, x'') -> Node (unfold f x') y (unfold f x'') 
    Nothing           -> Leaf

-- 2.

treeBuilder :: Integer -> BinaryTree Integer
treeBuilder n = unfold spreader n
  where
    spreader 0 = Nothing
    spreader x = Just (x - 1, n - x, x - 1)
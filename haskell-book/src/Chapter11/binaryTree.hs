-- Binary Trees, Page 444

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

-- Map

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "Yup, okay!"
  else error "Test failed, dingus."

-- Convert to list

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = (a : preorder left) ++ preorder right

-- preorder Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
-- (2: preorder (Node Leaf 1 Leaf)) ++ preorder (Node Leaf 3 Leaf)
-- (2: (1: preorder Leaf ++ preorder Leaf)) ++ (3 : preorder Leaf) ++ preorder Leaf
-- (2 : 1 : [] ++ []) ++ (3 : []) ++ []
-- [2, 1, 3]

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ (a : inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = 
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears, dingus."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears, dingus."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Postorder failed check, dingus."

-- Foldr

testTree'' :: BinaryTree Integer
testTree'' = (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 (Node Leaf 4 Leaf)))

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = foldTree f (f a (foldTree f b left)) right

-- foldTree (+) 0 testTree''

-- foldTree (+) ((+) 2 (foldTree (+) 0 (Node Leaf 1 Leaf))) (Node Leaf 3 (Node Leaf 4 Leaf)))
-- foldTree (+) ((+) 2 (foldTree (+) ((+) 1 (foldTree (+) 0 Leaf)) Leaf) (Node Leaf 3 (Node Leaf 4 Leaf)))
-- foldTree (+) ((+) 2 (foldTree (+) ((+) 1 0) Leaf)) (Node Leaf 3 (Node Leaf 4 Leaf)))
-- foldTree (+) ((+) 2 (foldTree (+) 1 Leaf)) (Node Leaf 3 (Node Leaf 4 Leaf)))
-- foldTree (+) ((+) 2 1) (Node Leaf 3 (Node Leaf 4 Leaf)))
-- foldTree (+) 3 (Node Leaf 3 (Node Leaf 4 Leaf)))

-- foldTree (+) ((+) 3 (foldTree (+) 3 Leaf)) (Node Leaf 4 Leaf)
-- foldTree (+) ((+) 3 3) (Node Leaf 4 Leaf)
-- foldTree (+) 6 (Node Leaf 4 Leaf)

-- foldTree (+) ((+) 4 (foldTree (+) 6 Leaf)) Leaf
-- foldTree (+) ((+) 4 6) Leaf
-- foldTree (+) 10 Leaf

--10
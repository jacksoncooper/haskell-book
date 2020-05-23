-- Composition property of Traversable:

-- traverse (Compose . fmap g . f) =
-- Compose . fmap (traverse g) . traverse f

-- Let the structure being traversed be [1, 2, 3].
-- Let the functions 'f' and 'g' be 'Just' and '(: [])' respectively.

-- traverse (Compose . fmap g . f) [1, 2, 3]

-- From GHC Base 4.12.0.0:

-- instance Traversable [] where
--   traverse f = List.foldr cons_f (pure [])
--     where cons_f x ys = liftA2 (:) (f x) ys

-- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
-- liftA2 f x = (<*>) (fmap f x)

-- List.foldr cons_f (pure []) [1, 2, 3]

-- 3 `cons_f` pure []
-- liftA2 (:) ((Compose . fmap g . f) 3) (pure [])

--             ^
--             Compose . fmap g . f

-- (<*>) (fmap (:) (Compose $ Just [3])) (pure [])
-- (<*>) (Compose $ Just [(3 :)]) (pure [])
-- (<*>) (Compose $ Just [(3 :)]) (Compose $ Just [[]])
-- Compose (Just [[3]])

-- 2 `cons_f` (Compose $ Just [[3]])
-- (<*>) (Compose $ Just [(2 :)]) (Compose $ Just [[3]])
-- Compose (Just [[2, 3]])

-- ...

-- Compose $ Just [[1, 2, 3]]
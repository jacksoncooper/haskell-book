-- Page 1051

discriminatory :: Bool -> Int
discriminatory b =
  case b of
    False -> 0
    True -> 1

-- In GHCi, with

-- :set -ddump-simpl
-- :set dsuppress-all

-- RHS size: {terms: 9, types: 2, coercions: 0, joins: 0/0}
-- discriminatory
-- discriminatory
--   = \ b_a1yq ->
--       case b_a1yq of {
--         False -> I# 0#;
--         True -> I# 1#
--       }

-- In Core, a case expression always evaluates what it cases on. So 'b_a1yq' is
-- always evaluated.

-- Core and Haskell are not the same language.

-- Page 1057

-- Evaluate.

-- 1. const 1 undefined
--    (\a -> (\b -> a)) 1 undefined
--    (\b -> 1) undefined
--    1
--    *not bottom*

-- 2. const undefined 1
--    (\a -> (\b -> a)) undefined 1
--    (\b -> undefined) 1
--    undefined
--    *oh no bottom*

-- 3. flip const undefined 1
--    (\(a -> b -> c) -> (\b -> (\a -> c))) (\d -> (\e -> d)) undefined 1
--    (\e -> (\d -> \d)) undefined 1
--    (\d -> d) 1
--    1
--    *not bottom*

-- 4. flip const 1 undefined
--    (\e -> (\d -> \d)) 1 undefined
--    (\d -> \d) undefined
--    undefined
--    *oh no bottom*

-- 5. const undefined undefined
--    (\a -> (\b -> a)) undefined undefined
--    (\b -> undefined) undefined
--    undefined
--    *very bottom*

-- 6. foldr const 'z' ['a'..'e']
--    'a' `const` ('b' `const` ('c' `const` ('d' `const` ('e' `const` 'z'))))
--    'a'
--    *not bottom yay*

-- 7. foldr (flip const) 'z' ['a'..'e']
--    'z'
--    *not bottom*


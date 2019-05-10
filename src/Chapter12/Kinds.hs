-- Chapter Exercises, Page 477

module Chapter12.Kinds where

-- 1. The kind of 'a' in 'id :: a -> a' is '*'. For any application of the 'id'
--    function, such as 'id "whoa"', the type variable 'a' is only capable of
--    being constrained to a type constant.

-- 2. The kind of 'f' in 'r :: a -> f a' is '* -> *'. The kind of 'a' is '*'.
--    The function 'r' takes something of type type-constant 'a' and returns
--    something of the type produced by the type constructor 'f' applied to the
--    type constant 'a'.
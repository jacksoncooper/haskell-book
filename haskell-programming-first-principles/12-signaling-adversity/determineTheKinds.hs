-- Chapter Exercises, p.477

-- 1. The kind of 'a' in id :: a -> a is *. For any application of the 'id'
--    function, such as id "whoa", the type variable 'a' is only capable of
--    being constrained to a type constant. id "whoa" :: String, and so 'a'
--    is constrained to the type constant String.

-- 2. The kind of 'f' in r :: a -> f a is * -> *. The kind of 'a' is *. The
--    function r takes something of type type-constant 'a' and returns
--    something of the type produced by the type constructor 'f' applied to the
--    type constant 'a'.
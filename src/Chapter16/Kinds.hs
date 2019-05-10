-- Page 623

module Chapter16.Kinds where

-- 1. The kind of 'a' in 'f :: a -> a' is '*', because the kind of (->) is
--    '* -> * -> *'. To construct a complete type, (->) takes two type constants,
--    i.e., two type arguments of kind '*'. In the type signature, each of those
--    arguments is of type 'a', so 'a' must be of kind '*'.

-- 2. The kind of 'b' in 'f :: a -> b a -> T (b a)' is '* -> *'. 'b' is a type
--    constructor that takes a type 'a' to construct a complete type. The kind
--    of 'T' is '* -> *' because it is a type constructor that takes a type, in
--    this case, (b a) of kind '*'.

-- 3. The kind of 'c' in 'f :: c a b -> c b a' is '* -> * -> *'.
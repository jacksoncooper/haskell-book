-- Exercises: Eq Instances p.180

-- 1.

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn y) = x == y

-- 2.

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two w x) (Two y z) = w == y && x == z

-- 3.

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt y) = x == y
    (==) (TisAString x) (TisAString y) = x == y
    (==) _ _ = False

-- 4.

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair w x) (Pair y z) = w == y && x == z

-- 5.

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple w x) (Tuple y z) = w == y && x == z

-- 6.

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne y) = x == y
    (==) (ThatOne x) (ThatOne y) = x == y
    (==) _ _ = False

-- 7.

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello y) = x == y
    (==) (Goodbye x) (Goodbye y) = x == y
    (==) _ _ = False


-- Exercises: Tuple Experiment p.182

-- divMod :: Integral a => a -> a -> (a, a)
-- divMod x y = (x `div` y, x `mod` y)

-- quotRem :: Integral a => a -> a -> (a, a)
-- quotRem x y = (x `quot` y, x `rem` y)


-- Exercises: Will They Work p.194

-- 1. Will work, returns 5. The max function has the type Ord a => a -> a -> a.
--    There is a type class constraint on all instances of the Ord type class,
--    namely Eq a => a. Because Int has an instance of the Eq and Ord type
--    classes, and the length function returns an Int, the expression
--    max (length [1, 2, 3]) (length [8, 9, 10, 11, 12]) will type check.

-- 2. Will work. The compare function has the type Ord a => a -> a -> Ordering.
--    The expressions (3 * 4) and (3 * 5) default to Integer's, and the Integer
--    type has an instance of the Ord type class. The expression
--    compare (3 * 4) (3 * 5) evaluates to LT.

-- 3. Will not work. The compare function requires that both arguments be of
--    type Ord a => a. "Julie" :: String and True :: Bool.

-- 4. Will work. The (>) function has type Ord a => a -> a -> Bool. The
--    expressions (5 + 3) and (3 + 6) default to Integer's, so the expression
--    (5 + 3) > (3 + 6) evaluates to False.


-- Chapter Exercises p.207

-- Multiple choice

-- 1. C
-- 2. B
-- 3. A
-- 4. C
-- 5. A

-- Does it typecheck

-- 1. Does not typecheck. Although the Bool argument to the person data
--    constructor has an instance of Show, Person has no instance of Show.
--    Fixed below.

data Person = Person Bool
    deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn $ show person

-- 2. Does not typecheck. Mood has no instance of Eq, so the comparison
--    x == Woot in the settleDown function will not be resolvable. Fixed below.

data Mood = Blah | Woot
    deriving (Eq, Show)

settleDown :: Mood -> Mood
settleDown x = if x == Woot then Blah else x

-- 3A. Anything of the Mood type is an acceptable input to the settleDown
--     function. The input is constrained by the expression x == Mood. The
--     (==) function has the type a -> a -> Bool, so the type of x == Mood is
--     Mood -> Bool, so x :: Mood.
-- 3B. Trying to evaluate settleDown 9 will result in an error complaining that
--     the function expected a value of type Mood but received one of type
--     Num a => a.
--     Incorrect: Haskell seems to try to constrain the polymorphic type
--     Num a => a to Num Mood, to see if a Mood can behave as a Num. This
--     fails because there is no such instance.
-- 3C. The expression Blah > Woot fails because Mood has no Ord instance.

-- 4. Will typecheck.

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object
    deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
-- ^ This does not "show" though; partially applied function.

s2 = Sentence "Julie" "loves" "dogs"

-- Given a datatype declaration, what can we do

-- 1. Will not typecheck. "chases" :: String, not Rocks and True :: Bool. not
--    Yeah.
-- 2. Will typecheck.
-- 3. Will typecheck.
-- 4. Will not typecheck. Papu has no Ord instance, nor do Rocks and Yeah.

-- Match the types

-- 1. The second type can be substituted without error. The polymorphic type
--    a is more general than Num a => a, so the definition i = 1 can conform
--    to both signatures.
--    Incorrect: i is strictly defined as 1, so it must be that i is of type
--    Num a => a and cannot be anything more general.
-- 2. The second type can be substituted without error. 1.0 can function both
--    as a Float and the polymorphic Num type, because 1.0 could represent
--    a Double, Float, etc.
--    Incorrect: Sheesh, strike two. The Num typleclass includes things like
--    Num Integer, which do not apply to 1.0. A more specific constraint is
--    needed. GHC infers Fractional a => a.
-- 3. The second type can be substituted without error, see #2.
-- 4. The second type can be substituted without error. The type RealFrac
--    has the constraint (Real a, Fractional a) => RealFrac a. 1.0 is both
--    real and fractional.
-- 5. The second type can be substituted without error. x is simply restricted
--    to the orderable types.
-- 6. The second type can be substituted without error. x is restricted to the
--    integers.
-- 7. The second type cannot be substituted without error. Because sigmund
--    always returns an Int, it cannot conform to a return type that is
--    parametrically polymorpic, i.e., a -> a.
-- 8. The second type can be substituted without error. Num a => a -> a is
--    constrained to Int -> Int because sigmund' returns an Int.
--    Incorrect: I keep thinking that the substitution works if the definition
--    conforms to the signature, and forget that all of the other inputs must
--    conform as well. This does not work because the function will only return
--    Int's, so passing in a Num Float or Num Double will never yield
--    correctly.
-- 9. The second type can be substituted without error, because Int has an
--    instance or Ord.

-- 10. The second type can be substituted without error, because the Ord
--     constraint prevents the domain from expanding into unsortable
--     objects.
-- 11. The second type cannot be substituted without error. The polymorphic
--     constraint [a] in the input of signifier would allow lists of things
--     other than Char's to be passed into mySort.

-- Type-Kwon-Do Two: Electric Boogaloo

-- 1.

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = aToB a == b

-- 2.

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB n a = aToB a + fromInteger n
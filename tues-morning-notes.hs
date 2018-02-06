x :: Integer -- types start with an uppercase letter
x = 3


f :: Integer -> Integer
f a = a + 99

-- Inline?? /a = lambda a
ff :: Integer -> Integer
ff = \a -> a + 99


fff :: Integer -> Integer -> Integer
fff a b = (a + b) * 2

ffff :: Integer -> Integer -> Integer
ffff = \a b -> (a + b) * 2
-- also equivalent to
ffffb = \a -> \b -> (a + b) * 2
-- can also be written as
(.+.) :: Integer -> Integer -> Integer
(.+.) = \a b -> (a + b) * 2

-- INFIX vs PREFIX
-- can call:
-- > ffff 5 6
-- > 5 `ffff` 6
-- > (.+.) 5 6
-- > 5 .+. 6
-- which is why these both work:
-- > 5 + 6
-- > (+) 5 6


-- type guess: (a -> a) -> Integer ?
fffff :: (Integer -> Integer) -> Integer
fffff k = k 33
-- can be invoked in many ways:
a = fffff (\name -> name + 10)
b = fffff ff
c = fffff (fff 4)


-- "generic" types aka parametric polymorphism
g :: t -> t -- type variables always start with a lowercase letter
g a = a -- gets the type t -> t
-- g a = a + 3 won't compile because that implies a must be an Integer, 
-- but we said it must be generic


-- how to make datatypes:
-- datatype names are capitalised
-- constructuor names are capitalised or start with ???
pie = 3
-- :t Circle :: Integer -> Shape
data Shape = 
    Circle Integer 
    | Rectangle Integer Integer
    | Triangle Integer Integer Integer
    -- deriving (Eq, Show) -- without Show >Circle 8 throws an error about 'can't show'
                        -- without Eq >Circle 8 == Circle 8 throws an error
                        -- deriving tells it to use the default implementation
    deriving Show -- so we can use Equal below

square :: Integer -> Shape
square x = Rectangle x x


-- how to make type classes:
class Equal a where
    (===) :: a -> a -> Bool

-- make an instance of Equal for Shape
instance Equal Shape where
    -- Shape -> Shape -> Bool
    (===) (Circle r1) (Circle r2) = r1 == r2 -- if Circles, grab r1 and r2 and compare them
    (===) _ _ = False
-- this uses algebraic data types

perimeter :: Shape -> Integer
-- perimeter = error "hi" -- throws an error
-- perimeter = _ -- throws an error that tells you about holes in your code
perimeter = \shape -> case shape of 
                        Circle r -> r * 2 * pie
                        Rectangle w h -> (w + h) * 2
                        Triangle a b c -> a + b + c
-- this is pattern matching

perimeteragain :: Shape -> Integer
perimeteragain (Circle r) = r * 2 * pie
perimeteragain (Rectangle w h) = (w + h) * 2
perimeteragain (Triangle a b c) = a + b + c 
-- better pattern matching using shorter syntax
-- this is exhaustive pattern matching because it matches all the patterns that exist
-- can turn on warnings about incomplete pattern matching using
-- >:set -fwarn-incomplete-patterns
-- this is also non-overlapped pattern matching, whereas using _ at any point would
-- be overlapping


data ExactlyTwo a = 
    -- a -> a -> ExactlyTwo a
    Two a a 
    deriving Show

class ThingsThatMap ttm where 
    mapThing :: (a -> b) -> ttm a -> ttm b

instance ThingsThatMap ExactlyTwo where 
    -- (a -> b) -> ExactlyTwo a -> ExactlyTwo b
    mapThing = \a2b othername -> case othername of 
        -- Two a1 a2 -> Two _x _y -- tells you about holes and names them!
        Two a1 a2 -> Two (a2b a1) (a2b a2)

data ExactlyOne a = One a deriving Show 

instance ThingsThatMap ExactlyOne where 
    mapThing f (One a) = One (f a)
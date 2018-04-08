module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where

    show Zero     = "0"
    show (Succ n) = "S" ++ show n

instance Eq Nat where

    Zero   == Zero    = True
    Zero   == Succ _  = False
    Succ _ == Zero    = False
    Succ m == Succ n  = m == n

instance Ord Nat where

    Zero <=  _ =  True
    Succ _ <= Zero = False
    Succ m <= Succ n = m <= n

    -- Ord does not require defining min and max.
    -- Howevener, you should define them without using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min Zero _ = Zero
    min _ Zero = Zero
    min (Succ m) (Succ n) = min m n

    max = undefined

isZero :: Nat -> Bool
isZero Zero = True
isZero _    = False

pred :: Nat -> Nat
pred Zero     = Zero
pred (Succ x) = x

even :: Nat -> Bool
even Zero            = True
even (Succ Zero)     = False
even (Succ (Succ x)) = even x

odd :: Nat -> Bool
odd = not . even

-- addition
(<+>) :: Nat -> Nat -> Nat
Succ n <+> Zero = Succ n
Zero <+> Succ n = Succ n
Zero <+> Zero = Zero
Succ n <+> Succ m = Succ $ Succ $ m <+> n 


-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when subtraction returns a negative number.
(<->) :: Nat -> Nat -> Nat
Succ n <-> Zero = Succ n
Zero <-> Succ _ = error "Not-natural output"
Zero <-> Zero = Zero
Succ Zero <-> Succ Zero = Zero
Succ n <-> Succ m = n <-> m

-- multiplication
(<*>) :: Nat -> Nat -> Nat
Zero <*> _ = Zero
_ <*> Zero = Zero
Succ Zero <*>  n = n
n <*> Succ Zero = n
Succ n <*> Succ m = Succ n <+> (Succ n <*> m)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
_ <^> Zero = Succ Zero
Zero <^> _ = Zero
Succ m <^> Succ Zero = Succ m
Succ m <^> Succ (Succ Zero) = Succ m <*> Succ m
Succ m <^> Succ n = Succ m <*> (Succ m <^> n)

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> Zero = error "Division by zero not allowed"
Zero </> _ = Zero

Succ Zero </> Succ Zero = Succ Zero
Succ Zero </> Succ n = error "error "
Succ n </> Succ Zero = Succ n
Succ n </> Succ m =  Succ Zero + (Succ n <-> Succ m) </> (Succ m) 

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) = undefined

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) = undefined

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful: here this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff = undefined

(|-|) = absDiff

factorial :: Nat -> Nat
factorial = undefined

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined


--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat = undefined

fromNat :: Integral a => Nat -> a
fromNat = undefined


-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
        | x < 0     = undefined
        | x == 0    = undefined
        | otherwise = undefined

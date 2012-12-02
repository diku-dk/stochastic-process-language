{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

module Language.SPL where

import Prelude hiding (Real, return, (>>=))

import Language.SPL.Operator
import Language.SPL.Syntax hiding (Duration, Time)
import qualified Language.SPL.Syntax as Syntax

import Control.RMonad.AsMonad

-- Types

type Duration = Syntax.Duration

type Time = Syntax.Time

type Real = Sample RealValue

type Boolean = Sample Bool

type Trace a = Time -> a

type Dist a = AsMonad Distribution a

type Process a = Dist (Trace a)

type Like a = Sample (Result a)

-- Smart constructors

-- Exclusive
scanWithTime :: forall a b. (Symbolic a, Symbolic b) => 
    Trace b -> a -> (Like a -> Time -> Duration -> Like b -> a) -> Trace (Like a)
scanWithTime r z f = Lookup (Scan (symbolic r') z' f')
    where
        r' :: Time -> Like b
        r' = symbolic . r
        z' :: Like a
        z' = symbolic z
        f' :: Like a -> Time -> Duration -> Like b -> Like a
        f' a t dt b = symbolic (f a t dt b)

-- Exclusive
scan :: (Symbolic a, Symbolic b) => 
    Trace b -> a -> (Like a -> Like b -> a) -> Trace (Like a)
scan r z f = scanWithTime r z (\a _ _ b -> f a b)

-- Inclusive
iterate :: (Symbolic a, Symbolic (Time -> a)) => 
        a -> (a -> Time -> Duration -> Dist a) -> Dist (Time -> a)
iterate z f = embed (Iterate z (\a t dt -> unEmbed (f a t dt)))

uniform :: Dist Real
uniform = embed Uniform

pair :: Sample a -> Sample b -> Sample (a, b)
pair = Binary Pair

-------------------- Pairs ----------

class Pair a where
    type First a
    type Second a
    first :: a -> First a
    second :: a -> Second a

instance (Symbolic a, Symbolic b) => Pair (a, b) where
    type First (a, b) = Sample (Result a)
    type Second (a, b) = Sample (Result b)
    first (a, b) = symbolic a
    second (a, b) = symbolic b

instance Pair (Sample (a, b)) where
    type First (Sample (a, b)) = Sample a
    type Second (Sample (a, b)) = Sample b
    first = Unary First
    second = Unary Second

instance Pair a => Pair (Trace a) where
    type First (Trace a) = Trace (First a)
    type Second (Trace a) = Trace (Second a)
    first f t = first (f t)
    second f t = second (f t)

-------------------- Comparison and logic ----------

infixl 4 .==., ./=., .<., .>., .<=., .>=.
infixl 3 .&&.
infixl 2 .||.

true :: Boolean
true = Boolean True

false :: Boolean
false = Boolean False

class Comparable a where
    when :: a Bool -> a b -> a b -> a b
    min, max :: a RealValue -> a RealValue -> a RealValue
    (.==.), (./=.), (.<.), (.>.), (.<=.), (.>=.) :: a RealValue -> a RealValue -> a Bool
    (.&&.), (.||.) :: a Bool -> a Bool -> a Bool

instance Comparable Sample where
    when = If
    min = Binary Min
    max = Binary Max
    (.==.) = Binary Equal 
    (./=.) = Binary NotEqual
    (.<.) = Binary Less
    (.>.) = Binary Greater
    (.<=.) = Binary LessEqual
    (.>=.) = Binary GreaterEqual
    (.&&.) = Binary And
    (.||.) = Binary Or

-------------------- Arithmetic ----------

instance Eq Real where
    (==) = error "Eq is not defined for Dist Real, but is still instantiated to provide Num (Dist Real)"

instance Show Real where
    show = error "Show is not defined for Dist Real, but is still instantiated to provide Num (Dist Real)"

instance Num Real where
    fromInteger = Real . fromInteger
    (+) = Binary Add
    (-) = Binary Sub
    (*) = Binary Mult
    abs = Unary Abs
    signum = Unary Sign
    negate = Unary Negate

instance Fractional Real where
    fromRational = Real . fromRational
    (/) = Binary Div
    recip = (1.0 /)

instance Floating Real where
    exp = Unary Exp
    pi = Real pi
    log = Unary Log
    sqrt = Unary Sqrt
    sin = Unary Sin
    cos = Unary Cos
    tan = Unary Tan
    asin = Unary Asin
    acos = Unary Acos
    atan = Unary Atan
    sinh = Unary Sinh
    cosh = Unary Cosh
    tanh = Unary Tanh
    asinh = Unary Asinh
    acosh = Unary Acosh
    atanh = Unary Atanh
    (**) = Binary Power
    logBase = Binary LogBase

instance Num a => Num (Trace a) where
    fromInteger i t = fromInteger i
    (+) a b t = a t + b t
    (-) a b t = a t - b t
    (*) a b t = a t * b t
    abs a t = abs (a t)
    signum a t = signum (a t)
    negate a t = negate (a t)

instance Floating a => Floating (Trace a) where
    exp a t = exp (a t)
    pi t = pi
    log a t = log (a t)
    sqrt a t = sqrt (a t)
    sin a t = sin (a t)
    cos a t = cos (a t)
    tan a t = tan (a t)
    asin a t = asin (a t)
    acos a t = acos (a t)
    atan a t = atan (a t)
    sinh a t = sinh (a t) 
    cosh a t = cosh (a t)
    tanh a t = tanh (a t)
    asinh a t = asinh (a t)
    acosh a t = acosh (a t)
    atanh a t = atanh (a t)
    (**) a p t = (a t ** p t)
    logBase a b t = logBase (a t) (b t)

instance Fractional a => Fractional (Trace a) where
    fromRational r t = fromRational r
    a / b = \t -> a t / b t
    recip a = \t -> recip (a t)

instance Show a => Show (Trace a) where
    show = error "Show is not defined for Trace Real, but is still instantiated to provide Num (Trace Real)"

instance Eq a => Eq (Trace a) where
    (==) = error "Eq is not defined for Trace Real, but is still instantiated to provide Num (Trace Real)"

---------- Show for Distribution --------

instance Show (Distribution a) where
    show (Return s) = "Return _"
    show Uniform = "Uniform"
    show (Iterate z f) = "Iterate _ (" ++ show (f undefined undefined undefined) ++ ")"
    show (Bind a f) = "Bind (" ++ show a ++ ") (" ++ show (f undefined) ++ ")"


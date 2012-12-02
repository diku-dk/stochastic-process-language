{-# LANGUAGE GADTs, KindSignatures, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, TypeFamilies, MultiParamTypeClasses, EmptyDataDecls, ScopedTypeVariables #-}

module Language.SPL.Syntax where

import Prelude hiding (fmap, (>>=), return)
import Language.SPL.Operator
import Control.RMonad
import Data.Suitable

type Duration = Sample RealValue -- The length of a time interval

type Time = Duration -- Offset from time zero

type Internal a = Sample a

data Sample :: * -> * where
    Real :: Double -> Sample RealValue
    Boolean :: Bool -> Sample Bool
    Unary :: UnaryOperator a b -> Sample a -> Sample b
    Binary :: BinaryOperator a b c -> Sample a -> Sample b -> Sample c
    If :: Sample Bool -> Sample a -> Sample a -> Sample a
    Scan :: Sample (Time -> b) -> Sample a -> (Sample a -> Time -> Duration -> Sample b -> Sample a) -> Sample (Time -> a)
    -- The following constructs are only supposed to be part of "Internal"
    Trace :: (Time -> Sample a) -> Sample (Time -> a)
    Lookup :: Sample (Time -> a) -> Time -> Sample a
    IterateLoop :: Sample a -> (Sample a -> Time -> Duration -> Sample a) -> Sample (Time -> a)
    GenerateUniform :: Sample RealValue
    Let :: Sample a -> (Sample a -> Sample b) -> Sample b
    Variable :: Symbols a -> Sample a

data Distribution :: * -> * where

    Uniform :: Distribution (Sample RealValue)

    Iterate :: 
        a -> 
        (a -> Time -> Duration -> Distribution a) -> 
        Distribution (Time -> a)

    Bind :: (Symbolic a) =>
        Distribution a -> 
        (a -> Distribution b) -> 
        Distribution b
        
    Return :: a -> Distribution a

-- Symbols are only used internally

data Symbols :: * -> * where
    NowSymbols :: Symbols RealValue
    DeltaSymbols :: Symbols RealValue
    ArgumentSymbols :: Int -> Symbols RealValue
    RealSymbols :: Int -> Symbols RealValue
    BoolSymbols :: Int -> Symbols Bool
    PairSymbols :: Symbols a -> Symbols b -> Symbols (a, b)
    TraceSymbols :: Symbols a -> Symbols (Time -> a)

-- Monads and functors

data instance Constraints Distribution a = Symbolic a => DistributionConstraints

instance Symbolic a => Suitable Distribution a where
    constraints = DistributionConstraints

instance RMonad Distribution where
    return a = 
        withResConstraints $ \DistributionConstraints -> 
        Return a
    s >>= f = 
        withConstraintsOf s $ \DistributionConstraints -> 
        withResConstraints $ \DistributionConstraints -> 
        Bind s f

instance RFunctor Distribution where
    fmap f a = 
        withResConstraints $ \DistributionConstraints -> 
        a >>= (Return . f)

-- Translation to symbolic representation

class Symbolic a where
    type Result a
    symbolic :: a -> Internal (Result a)
    function :: (a -> b) -> Internal (Result a) -> b
    distribution :: Distribution a -> Internal (Result a)

instance Symbolic (Sample a) where
    type Result (Sample a) = a
    symbolic = id
    function = id
    distribution d = case d of
        Uniform -> GenerateUniform
        Return a -> symbolic a
        Bind a f -> Let (distribution a) (\a -> distribution (function f a))

instance Symbolic (Sample a, Sample b) where
    type Result (Sample a, Sample b) = (a, b)
    symbolic (a, b) = Binary Pair (symbolic a) (symbolic b)
    function f x = f (Unary First x, Unary Second x)
    distribution = distribution . fmap symbolic

instance Symbolic (Time -> Sample a) where
    type Result (Time -> Sample a) = Time -> a
    symbolic f = Trace f
    function f x = f (Lookup x)
    distribution d = case d of
        Iterate z f -> IterateLoop (symbolic z) (\a t dt -> distribution (function f a t dt))
        Return a -> symbolic a
        Bind a f -> Let (distribution a) (\a -> distribution (function f a))


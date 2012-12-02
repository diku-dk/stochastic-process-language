{-# LANGUAGE GADTs, KindSignatures, EmptyDataDecls #-}

module Language.SPL.Operator where

data RealValue -- Real numbers

data UnaryOperator :: * -> * -> * where
    Negate :: UnaryOperator RealValue RealValue
    Abs :: UnaryOperator RealValue RealValue
    Sign :: UnaryOperator RealValue RealValue
    Exp :: UnaryOperator RealValue RealValue
    Log :: UnaryOperator RealValue RealValue
    Sqrt :: UnaryOperator RealValue RealValue
    Sin :: UnaryOperator RealValue RealValue
    Cos :: UnaryOperator RealValue RealValue
    Tan :: UnaryOperator RealValue RealValue
    Asin :: UnaryOperator RealValue RealValue
    Acos :: UnaryOperator RealValue RealValue
    Atan :: UnaryOperator RealValue RealValue
    Sinh :: UnaryOperator RealValue RealValue
    Cosh :: UnaryOperator RealValue RealValue
    Tanh :: UnaryOperator RealValue RealValue
    Asinh :: UnaryOperator RealValue RealValue
    Acosh :: UnaryOperator RealValue RealValue
    Atanh :: UnaryOperator RealValue RealValue
    Not :: UnaryOperator Bool Bool
    First :: UnaryOperator (a, b) a
    Second :: UnaryOperator (a, b) b

data BinaryOperator :: * -> * -> * -> * where
    Add :: BinaryOperator RealValue RealValue RealValue
    Sub :: BinaryOperator RealValue RealValue RealValue
    Mult :: BinaryOperator RealValue RealValue RealValue
    Div :: BinaryOperator RealValue RealValue RealValue
    Power :: BinaryOperator RealValue RealValue RealValue
    LogBase :: BinaryOperator RealValue RealValue RealValue
    Min :: BinaryOperator RealValue RealValue RealValue
    Max :: BinaryOperator RealValue RealValue RealValue
    Less :: BinaryOperator RealValue RealValue Bool
    LessEqual :: BinaryOperator RealValue RealValue Bool
    Greater :: BinaryOperator RealValue RealValue Bool
    GreaterEqual :: BinaryOperator RealValue RealValue Bool
    Equal :: BinaryOperator RealValue RealValue Bool
    NotEqual :: BinaryOperator RealValue RealValue Bool
    And :: BinaryOperator Bool Bool Bool
    Or :: BinaryOperator Bool Bool Bool
    Pair :: BinaryOperator a b (a, b)


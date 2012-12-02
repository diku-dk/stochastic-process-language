{-# LANGUAGE GADTs, KindSignatures, TypeSynonymInstances, TypeFamilies, FlexibleInstances #-}

module Language.SPL.Translate where

import Prelude hiding (Real)
import qualified Prelude as P
import Language.SPL.Operator
import Language.SPL.Syntax
import Language.SPL
import Control.Monad
import Control.Monad.State
import Data.List (intercalate)
import Control.RMonad.AsMonad



class Translatable a where
    translateFunction :: a -> [Symbols RealValue] -> String
    
instance Translatable (Process Real) where
    translateFunction p arguments = translateProcess arguments p

instance Translatable a => Translatable (Real -> a) where
    translateFunction p arguments = 
        let s = ArgumentSymbols (length arguments) in
        translateFunction (p (Variable s)) (s : arguments)

translateProcess :: [Symbols RealValue] -> Process Real -> String
translateProcess arguments d = translateDist arguments $ do 
    t <- d
    return (t (Variable NowSymbols))

translateDist :: [Symbols RealValue] -> Dist Real -> String
translateDist arguments d = translateProgram arguments (distribution (unEmbed d))

translateProgram :: [Symbols RealValue] -> Sample RealValue -> String
translateProgram arguments s =
    let (r, m) = runState (translate s) newGeneratorState in
    "#pragma OPENCL EXTENSION cl_khr_fp64 : enable\n" ++ 
    "#include \"random.cl\"\n" ++ 
    "#include \"sum.cl\"\n" ++ 
    "\n" ++ 
    "kernel void simulate(\n" ++ 
    "        ulong seed,\n" ++
    "        local double * local_means,\n" ++
    "        local double * local_standard_deviations,\n" ++
    "        global double * global_means,\n" ++
    "        global double * global_standard_deviations,\n" ++
    concat ["        double " ++ fromRealSymbol a ++ ",\n" | a <- arguments] ++
    "        double end, double delta) {\n" ++
    "    \n" ++ 
    "    double time;\n" ++
    "    int timeIndex;\n" ++
    "    int timeIndexCount = (int) (end / delta);\n" ++
    "    struct generator_t generator = initialize(seed);\n" ++
    "    \n" ++ 
    (if null (doubles m) then "" else "    double " ++ intercalate ", " (map (("x" ++ ) . show) (reverse $ doubles m)) ++ ";\n") ++
    (if null (bools m) then "" else "    bool " ++ intercalate ", " (map (("x" ++ ) . show) (reverse $ bools m)) ++ ";\n") ++
    "    \n" ++ 
    "    for(timeIndex = 0; timeIndex <= timeIndexCount; timeIndex++) {\n" ++
    "        time = timeIndex * delta;\n" ++
    "    \n" ++ 
    concat (reverse (code m)) ++
    "    }\n" ++
    "    \n" ++ 
    "    emit_result(\n" ++
    "       " ++ fromRealSymbol r ++ ",\n" ++
    "       local_means,\n" ++
    "       local_standard_deviations,\n" ++
    "       global_means,\n" ++
    "       global_standard_deviations\n" ++
    "    );\n" ++
    "}\n"
    

translate :: Sample a -> Generator (Symbols a)
translate s = case s of
    Real v -> doubleLiteral v
    Boolean v -> boolLiteral v
    Unary o a -> do
        let unaryReal :: String -> Sample RealValue -> Generator (Symbols RealValue)
            unaryReal o' a = do
                a' <- translate a
                b' <- freshDouble
                statement $ fromRealSymbol b' ++ " = " ++ o' ++ "(" ++ fromRealSymbol a' ++ ");"
                return b'
        case o of
            Negate -> unaryReal "-" a
            Abs -> unaryReal "fabs" a
            Sign -> unaryReal "sign" a
            Exp -> unaryReal "exp" a
            Log -> unaryReal "log" a
            Sqrt -> unaryReal "sqrt" a
            Sin -> unaryReal "sin" a
            Cos -> unaryReal "cos" a
            Tan -> unaryReal "tan" a
            Asin -> unaryReal "asin" a
            Acos -> unaryReal "acos" a
            Atan -> unaryReal "atan" a
            Sinh -> unaryReal "sinh" a
            Cosh -> unaryReal "cosh" a
            Tanh -> unaryReal "tanh" a
            Asinh -> unaryReal "asinh" a
            Acosh -> unaryReal "acosh" a
            Atanh -> unaryReal "atanh" a
            Not -> do
                a' <- translate a
                b' <- freshBool
                statement $ fromBoolSymbol b' ++ " = !" ++ fromBoolSymbol a' ++ ";"
                return b'
            First -> do
                PairSymbols a' _ <- translate a
                return a'
            Second -> do
                PairSymbols _ a' <- translate a
                return a'
    Binary o a b -> do
        let binaryReal' :: (String -> String -> String) -> Sample RealValue -> Sample RealValue -> Generator (Symbols RealValue)
            binaryReal' o' a b = do
                a' <- translate a
                b' <- translate b
                r' <- freshDouble
                statement $ fromRealSymbol r' ++ " = " ++ o' (fromRealSymbol a') (fromRealSymbol b') ++ ";"
                return r'
            binaryRealInfix o' a b = binaryReal' (\x y -> x ++ " " ++ o' ++ " " ++ y) a b
            binaryReal o' a b = binaryReal' (\x y -> o' ++ "(" ++ x ++ ", " ++ y ++ ")") a b
            binaryRelation :: String -> Sample RealValue -> Sample RealValue -> Generator (Symbols Bool)
            binaryRelation o' a b = do
                a' <- translate a
                b' <- translate b
                r' <- freshBool
                statement $ fromBoolSymbol r' ++ " = " ++ o' ++ "(" ++ fromRealSymbol a' ++ ", " ++ fromRealSymbol b' ++ ");"
                return r'
        case o of
            Add -> binaryRealInfix "+" a b
            Sub -> binaryRealInfix "-" a b
            Mult -> binaryRealInfix "*" a b
            Div -> binaryRealInfix "/" a b
            Min -> binaryReal "min" a b
            Max -> binaryReal "max" a b
            Power -> binaryReal "pow" a b
            LogBase -> do
                a' <- translate a
                b' <- translate b
                r' <- freshDouble
                statement $ fromRealSymbol r' ++ " = log(" ++ fromRealSymbol b' ++ ") / log(" ++ fromRealSymbol a' ++ ");"
                return r'
            Less -> binaryRelation "isless" a b
            LessEqual -> binaryRelation "islessequal" a b
            Greater -> binaryRelation "isgreater" a b
            GreaterEqual -> binaryRelation "isgreaterequal" a b
            Equal -> binaryRelation "isequal" a b
            NotEqual -> binaryRelation "isnotequal" a b
            And -> do
                a' <- translate a
                b' <- translate b
                r' <- freshBool
                statement $ fromBoolSymbol r' ++ " = " ++ fromBoolSymbol a' ++ " & " ++ fromBoolSymbol b' ++ ";"
                return r'
            Or -> do
                a' <- translate a
                b' <- translate b
                r' <- freshBool
                statement $ fromBoolSymbol r' ++ " = " ++ fromBoolSymbol a' ++ " | " ++ fromBoolSymbol b' ++ ";"
                return r'
            Pair -> do
                a' <- translate a
                b' <- translate b
                return (PairSymbols a' b')
    If a b c -> do
        a' <- translate a
        statement $ "if(" ++ fromBoolSymbol a' ++ ") {"
        b' <- indent $ translate b
        r' <- uninitializedSymbols b'
        indent $ assign r' b'
        statement $ "} else {"
        c' <- indent $ translate c
        indent $ assign r' c'
        statement $ "}"
        return r'
    Lookup t (Variable NowSymbols) -> do
        TraceSymbols t' <- translate t
        return t'
    Lookup t w -> error "Lookup at different times are not currently supported"
    GenerateUniform -> generateUniform
    Let a f -> do
        a' <- translate a
        translate (f (Variable a'))
    Variable i -> return i
    Scan t z f -> do
        z' <- onlyFirstIteration $ translate z
        t' <- translate t
        f' <- translate (f (Variable z') (Variable NowSymbols) (Variable DeltaSymbols) (Variable (unTrace t')))
        z' `assign` f'
        return (TraceSymbols z')
    Trace f -> liftM TraceSymbols $ translate (f (Variable NowSymbols))
    IterateLoop z f -> do
        z' <- onlyFirstIteration $ translate z
        nonFirstIteration $ do
            f' <- translate (f (Variable z') (Variable NowSymbols) (Variable DeltaSymbols))
            z' `assign` f'
        return (TraceSymbols z')

unTrace :: Symbols (Syntax.Time -> a) -> Symbols a
unTrace (TraceSymbols s) = s

assign :: Symbols a -> Symbols a -> Generator ()
RealSymbols a `assign` b = statement $ fromRealSymbol (RealSymbols a) ++ " = " ++ fromRealSymbol b ++ ";"
BoolSymbols a `assign` b = statement $ fromBoolSymbol (BoolSymbols a) ++ " = " ++ fromBoolSymbol b ++ ";"
PairSymbols a a' `assign` PairSymbols b b' = do
    a `assign` b
    a' `assign` b'
TraceSymbols a `assign` TraceSymbols b = a `assign` b

doubleLiteral :: P.Real a => a -> Generator (Symbols RealValue)
doubleLiteral a = do
    x <- freshDouble
    statement $ fromRealSymbol x ++ " = " ++ show a ++ ";"
    return x

boolLiteral :: Bool -> Generator (Symbols Bool)
boolLiteral a = do
    x <- freshBool
    statement $ fromBoolSymbol x ++ " = " ++ (if a then "true" else "false") ++ ";"
    return x

uninitializedSymbols :: Symbols a -> Generator (Symbols a)
uninitializedSymbols s = case s of
    NowSymbols -> freshDouble
    DeltaSymbols -> freshDouble
    RealSymbols _ -> freshDouble
    BoolSymbols _ -> freshBool
    PairSymbols a b -> liftM2 PairSymbols (uninitializedSymbols a) (uninitializedSymbols b)
    TraceSymbols a -> liftM TraceSymbols (uninitializedSymbols a)

data GeneratorState = GeneratorState {
    nextVariable :: Int,
    doubles :: [Int],
    bools :: [Int],
    indentation :: Int,
    code :: [String]
    }
    
type Generator a = State GeneratorState a

newGeneratorState = GeneratorState {
    nextVariable = 0,
    doubles = [],
    bools = [],
    indentation = 2,
    code = []
    }

fromRealSymbol :: Symbols RealValue -> String
fromRealSymbol NowSymbols = "time"
fromRealSymbol DeltaSymbols = "delta"
fromRealSymbol (RealSymbols v) = "x" ++ show v
fromRealSymbol (ArgumentSymbols v) = "a" ++ show v

fromBoolSymbol :: Symbols Bool -> String
fromBoolSymbol (BoolSymbols v) = "x" ++ show v

emit :: String -> Generator ()
emit c = modify (\s -> s { code = c : code s })

emitLine :: Int -> [String] -> Generator ()
emitLine i cs = do
    emit (replicate (i * 4) ' ')
    mapM_ emit cs
    emit "\n"

statement :: String -> Generator ()
statement cs = do
    i <- liftM indentation get
    emitLine i [cs]

indent m = do
    modify (\s -> s { indentation = indentation s + 1 })
    r <- m
    modify (\s -> s { indentation = indentation s - 1 })
    return r

freshDouble :: Generator (Symbols RealValue)
freshDouble = do
    i <- liftM nextVariable get
    modify (\s -> s { nextVariable = i + 1, doubles = i : doubles s })
    return (RealSymbols i)

freshBool :: Generator (Symbols Bool)
freshBool = do
    i <- liftM nextVariable get
    modify (\s -> s { nextVariable = i + 1, bools = i : bools s })
    return (BoolSymbols i)

generateUniform = do
    x <- freshDouble
    statement $ fromRealSymbol x ++ " = uniform(&generator);"
    return x

onlyFirstIteration m = do
    statement $ "if(timeIndex == 0) {"
    r <- indent m
    statement $ "}"
    return r

nonFirstIteration m = do
    statement $ "if(timeIndex != 0) {"
    r <- indent m
    statement $ "}"
    return r


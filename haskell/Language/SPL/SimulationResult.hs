{-# LANGUAGE ScopedTypeVariables #-}

module Language.SPL.SimulationResult where

data SimulationResult = SimulationResult {
    mean :: Double,
    standardDeviation :: Double
    }

expected :: SimulationResult -> Double
expected (SimulationResult e _) = e

standardDerivarion :: SimulationResult -> Double
standardDerivarion (SimulationResult _ s) = s

instance Show SimulationResult where
    show (SimulationResult expected standardDerivarion) = show expected ++ " ± " ++ show standardDerivarion

instance Read SimulationResult where
    readsPrec _ s = case reads s of
        [(expected :: Double, rest)] ->
            case splitAt 3 rest of
                (" ± ", rest) -> 
                    case reads rest of 
                        [(standardDeviation :: Double, rest)] ->
                            [(SimulationResult expected standardDeviation, rest)]
    
result results = 
    let n = fromIntegral (length results)
        expected = sum results / n
        deviations = map (\result -> (result - expected)^2) results
        standardDeviation = if n == 1 then 0 else sqrt (sum deviations / (n - 1))
    in SimulationResult expected standardDeviation

combine :: Int -> [Double] -> [Double] -> SimulationResult
combine _ [mean] [standardDeviation] = SimulationResult mean standardDeviation
combine elementSize means standardDeviations = SimulationResult mean standardDeviation
    where
        n = fromIntegral elementSize
        m = fromIntegral (length means)
        mean = sum means / m
        composed1 = sum (zipWith f1 means standardDeviations)
        f1 mean' standardDeviation' = (n - 1) * (standardDeviation' * standardDeviation') + n * (mean' * mean')
        composed2 = (m * n) * (mean * mean)
        standardDeviation = sqrt((composed1 - composed2) / (n * m - 1))

combineResults :: Int -> [SimulationResult] -> SimulationResult
combineResults elementSize results = 
    combine elementSize (map mean results) (map standardDeviation results)

data Comparator = Comparator {
    name :: String,
    compare :: SimulationResult -> SimulationResult -> Bool
    }

standardEquality = Comparator "Standard derivation equality" compare
    where
        epsilon = 0.00001
        compare (SimulationResult expected1 standardDeviation1) (SimulationResult expected2 standardDeviation2) = 
            abs (expected1 - expected2) <= max epsilon (max standardDeviation1 standardDeviation2)


epsilonEquality = Comparator "Epsilon equality" compare
    where
        epsilon = 0.00001
        compare (SimulationResult expected1 standardDeviation1) (SimulationResult expected2 standardDeviation2) = 
            abs (expected1 - expected2) <= epsilon && 
                abs (standardDeviation1 - standardDeviation2) <= epsilon

approximateEquality epsilon = Comparator ("Approximate equality (within 1% or " ++ show epsilon ++ ")") compare
    where
        factor = 0.01
        compare (SimulationResult expected1 standardDeviation1) (SimulationResult expected2 standardDeviation2) = 
            abs (expected1 - expected2) <= max epsilon (factor * abs (max expected1 expected2))


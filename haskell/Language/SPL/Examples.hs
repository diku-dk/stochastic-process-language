module Language.SPL.Examples where

import Prelude hiding (Real, min, max, iterate)
import Language.SPL

normal :: Dist Real
normal = do
    u1 <- uniform
    u2 <- uniform
    return (sqrt (-2 * log u1) * cos (2 * pi * u2))

brownian :: Process Real
brownian = iterate 0 $ \w t dt -> do
    n <- normal
    return (w + n * sqrt dt)

underlying :: Real -> Real -> Real -> Process Real
underlying r v s = do
    w <- brownian
    let u t = s * exp ((r - 0.5 * v^2) * t + v * w t)
    return u

average :: Trace Real -> Trace Real
average p t = 
    let v = scan p (0, 0) (\a v -> (first a + v, second a + 1)) in
    first (v t) / second (v t)

asian :: Real -> Real -> Real -> Process Real
asian r v s = do
    u <- underlying r v s
    return (\t -> max 0 (u t - average u t) * exp (-t * r))

irrational :: Real
irrational = sqrt 2 + pi


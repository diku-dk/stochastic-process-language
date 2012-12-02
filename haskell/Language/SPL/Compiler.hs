{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, TypeFamilies, FlexibleInstances #-}

module Language.SPL.Compiler where

import Foreign.OpenCL.Bindings
import Foreign hiding (mallocArray, peekArray, allocaArray)
import Foreign.C
import Control.Monad
import Control.Applicative
import Prelude hiding (lookup, zip, reverse, Real)
import qualified Prelude as P
import Data.Word
import System.Random
import Language.SPL.SimulationResult
import Language.SPL.Translate
import Language.SPL.SPL


class Function a where
    type FunctionOf a
    toFunction :: a -> [KernelArg] -> ([KernelArg] -> Double -> Double -> IO SimulationResult) -> FunctionOf a

instance Function (Process Real) where
    type FunctionOf (Process Real) = Double -> Double -> IO SimulationResult
    toFunction _ arguments f = f arguments

instance forall a. Function a => Function (Real -> a) where
    type FunctionOf (Real -> a) = Double -> FunctionOf a
    toFunction _ arguments f = 
        \v -> toFunction (undefined :: a) (VArg v : arguments) f


compile :: (Translatable a, Function a) => a -> IO (FunctionOf a)
compile p = compileSource p (translateFunction p [])


deviceTypes = [DeviceTypeGpu]
properties = [QueueOutOfOrderExecModeEnable]

groupSize = 512
groupCount = 512 * 4

compileSource :: Function a => a -> String -> IO (FunctionOf a)
compileSource p source = do
    platforms <- getPlatformIDs
    let platform = head platforms
    devices' <- getDeviceIDs deviceTypes platform
    let devices = take 1 (devices')
    context <- createContext devices [ContextPlatform platform] NoContextCallback
    queues <- mapM (\device -> createCommandQueue context device properties) devices
    program <- createProgram context source
    buildProgram program devices "-Werror -Iinclude"
    simulateKernels <- mapM (const (createKernel program "simulate")) queues
    resultKernels <- mapM (const (createKernel program "global_result")) queues
    let units = P.zip3 queues simulateKernels resultKernels
    let groupCount' = groupCount `div` length units -- TODO
    let workPerDevice = groupCount' * groupSize
    --------------------------
    return $ toFunction p [] $ \extraArguments time delta -> do
        let arguments = extraArguments ++ [VArg time, VArg delta]
        results <- mapM (enqueueKernel context groupCount' arguments) units
        let (means, standardDeviations) = unzip results
        return $ combine workPerDevice means standardDeviations
    where
        doubleSize = sizeOf (error "sizeOf" :: Double)
        enqueueKernel context groupCount' arguments (queue, simulateKernel, resultKernel) = do
            seed <- randomRIO (fromIntegral (minBound :: Word64), fromIntegral (maxBound :: Word64)) :: IO Integer
            let seed' = fromIntegral seed :: Word64
            
            let workPerDevice = groupCount' * groupSize
            
            meansBuffer <- mallocArray context [MemReadWrite] groupCount' :: IO (MemObject Double)
            standardDeviationsBuffer <- mallocArray context [MemReadWrite] groupCount' :: IO (MemObject Double)
            
            setKernelArgs simulateKernel ([
                VArg seed',
                LocalArrayArg (undefined :: Double) (fromIntegral $ groupSize), 
                LocalArrayArg (undefined :: Double) (fromIntegral $ groupSize), 
                MObjArg meansBuffer, 
                MObjArg standardDeviationsBuffer
                ] ++ arguments)
            simulateEvent <- enqueueNDRangeKernel queue simulateKernel [] 
                [fromIntegral $ workPerDevice] [fromIntegral $ groupSize] 
                []
            
            setKernelArgs resultKernel [
                VArg (fromIntegral $ groupSize :: Int32), 
                MObjArg meansBuffer, 
                MObjArg standardDeviationsBuffer]
            resultEvent <- enqueueNDRangeKernel queue resultKernel [] 
                [fromIntegral $ groupCount' `div` 2] [fromIntegral $ groupCount' `div` 2] 
                [simulateEvent]

            -- TODO this should not happen in the look as it blocks multiple devices to run in parallel.
            waitForEvents [resultEvent]
            --meansEvent <- enqueueReadBuffer queue meansBuffer False 0 (fromIntegral $ 1 * doubleSize) meansArray [resultEvent]
            --standardDeviationsEvent <- enqueueReadBuffer queue standardDeviationsBuffer False 0 (fromIntegral $ 1 * doubleSize) standardDeviationsArray [resultEvent]
            
            [mean] <- peekListArray queue 1 meansBuffer
            [standardDeviation] <- peekListArray queue 1 standardDeviationsBuffer
            
            return (mean, standardDeviation)


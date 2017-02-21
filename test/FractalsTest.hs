{-# LANGUAGE TemplateHaskell #-}

module FractalsTest
( runTests
) where

import Test.QuickCheck
import Data.Word
import Data.Complex

import Fractals

prop_toGrayscaleLength :: [Word8] -> Bool
prop_toGrayscaleLength ws = length (toGrayscale ws) == 4 * (length ws)

inMandelbrotSet :: Int -> Complex Double -> Bool
inMandelbrotSet nMax z0 = order (mandelbrot z0) 2.0 nMax z0 == nMax

prop_orderMandelbrotOutside :: Positive Int -> Complex Double -> Property
prop_orderMandelbrotOutside (Positive nMax) z0 =
    (magnitude z0 > 2.0) ==> not $ inMandelbrotSet nMax z0

prop_orderMandelbrotCardioid :: Positive Int    -- ^ the max number of iterations
                             -> Complex Double  -- ^ required to generate a value in the unit disk
                             -> Complex Double  -- ^ required to generate a value in the unit disk
                             -> Property
prop_orderMandelbrotCardioid (Positive nMax) w z =
    (magZ>0.0 || magW>0.0) ==> inMandelbrotSet nMax z0
    where z0 = 0.5 * mu * (1.0 - 0.5 * mu)
          magW = magnitude w
          magZ = magnitude z
          mu = if magW < magZ then fmap (/magZ) w else fmap (/magW) z

prop_orderMandelbrotCircle :: Positive Int    -- ^ the max number of iterations
                           -> Complex Double  -- ^ required to generate a value in the unit disk
                           -> Complex Double  -- ^ required to generate a value in the unit disk
                           -> Property
prop_orderMandelbrotCircle (Positive nMax) w z =
    (magZ>0.0 || magW>0.0) ==> inMandelbrotSet nMax z0
    where z0 = (-1.0) + (fmap (*0.25) mu)
          magW = magnitude w
          magZ = magnitude z
          mu = if magW < magZ then fmap (/magZ) w else fmap (/magW) z

return []
runTests :: IO Bool
runTests = $quickCheckAll

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

toUnitCircle :: RealFloat a => Complex a -> Complex a -> Complex a
toUnitCircle w z = if magW>0.0 && magZ>0.0 then mu else (0.0 :+ 0.0)
    where magW = magnitude w
          magZ = magnitude z
          mu = if magW < magZ then w/(magZ:+0.0) else z/(magW:+0.0)

prop_orderMandelbrotCardioid :: Positive Int    -- ^ the max number of iterations
                             -> Complex Double  -- ^ required to generate a value in the unit disk
                             -> Complex Double  -- ^ required to generate a value in the unit disk
                             -> Bool
prop_orderMandelbrotCardioid (Positive nMax) w z = inMandelbrotSet nMax z0
    where z0 = mandelbrotCardioid $ toUnitCircle w z

prop_orderMandelbrotCircle :: Positive Int    -- ^ the max number of iterations
                           -> Complex Double  -- ^ required to generate a value in the unit disk
                           -> Complex Double  -- ^ required to generate a value in the unit disk
                           -> Bool
prop_orderMandelbrotCircle (Positive nMax) w z = inMandelbrotSet nMax z0
    where z0 = (-1.0) + mu * 0.25
          mu = toUnitCircle w z

return []
runTests :: IO Bool
runTests = $quickCheckAll

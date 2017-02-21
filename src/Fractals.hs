module Fractals
    ( order
    , floatOrder
    , mandelbrot
    , bmp
    , toGrayscale
    , tangentPoint
    , mandelbrotCardioid
    , mag2
    ) where

import Data.Complex (Complex(..), mkPolar)
import Graphics.Gloss
import qualified Data.ByteString as B
import Data.Word

mag2:: Complex Double -> Double
mag2 (a:+b) = a^(2::Int) + b^(2::Int)

-- | compute the order of a given complex function
order :: (Complex Double -> Complex Double) -- ^ a function on the complex plane
      -> Double                             -- ^ the escape radius squared
      -> Int                                -- ^ the maximum order
      -> Complex Double                     -- ^ a starting point
      -> (Int, Complex Double)              -- ^ the order
order f radius2 nMax z0 = let f' = \ (i, z) -> (i+1, f z)
                              fs = dropWhile (\ (i, z) -> i<nMax && mag2 z < radius2) $ iterate f' (0, z0)
                           in head fs

-- | compute the order of a given complex function
floatOrder :: (Complex Double -> Complex Double) -- ^ a function on the complex plane
           -> Double                             -- ^ the escape radius squared
           -> Int                                -- ^ the maximum order
           -> Complex Double                     -- ^ a starting point
           -> Double                             -- ^ the order
floatOrder f radius2 nMax z0 = let (nu, z) = order f radius2 nMax z0
                                in fromIntegral nu - 0.5 * (log $ log $ mag2 z) / log 2.0

-- | the Mandelbrot function: z^2 + c
mandelbrot :: Complex Double -- ^ the c parameter
           -> Complex Double -- ^ the z variable
           -> Complex Double -- ^ the resulting value
mandelbrot (cr:+ci) (zr:+zi) = (cr + zr^(2::Int) - zi^(2::Int)) :+ (ci + 2*zr*zi)
--mandelbrot c z = z**2 + c

tangentPoint :: Rational -> Complex Double
tangentPoint r = mandelbrotCardioid $ mkPolar 1.0 (2.0 * pi * fromRational r)

mandelbrotCardioid :: Complex Double -> Complex Double
mandelbrotCardioid mu = 0.5 * mu * (1.0 - 0.5 * mu)

bitmapFormat :: BitmapFormat
bitmapFormat = BitmapFormat BottomToTop PxRGBA

normToWord8 :: Double -> Double -> Word8
normToWord8 maxX x = round (255 * x / maxX)

escapeRadius2 :: Double
escapeRadius2 = 9.0

maxIter :: Int
maxIter = 255

bmpValues :: Int -> Int -> Double -> Double -> Double -> [Word8]
bmpValues w h xc yc picScale =
    let wf = fromIntegral w * picScale
        hf = fromIntegral h * picScale
        floatOrderFromIJ :: Int -> Int -> Double
        floatOrderFromIJ i j = let x0 = xc - 0.5*wf + (fromIntegral j)*wf/(fromIntegral (w-1))
                                   y0 = yc - 0.5*hf + (fromIntegral i)*hf/(fromIntegral (h-1))
                                   z0 = x0 :+ y0
                                in floatOrder (mandelbrot z0) escapeRadius2 maxIter z0
        vals = floatOrderFromIJ <$> [0..h-1] <*> [0..w-1]
        maxVal = maximum vals
     in map (normToWord8 maxVal) vals

toGrayscale :: [Word8] -> [Word8]
toGrayscale [] = []
toGrayscale (x:xs) = x:x:x:255:(toGrayscale xs)

bmpByteString :: Int -> Int -> Double -> Double -> Double -> B.ByteString
bmpByteString w h xc yc picScale = B.pack vals
    where vals = toGrayscale $ bmpValues w h xc yc picScale

bmp :: Int -> Int -> Complex Double -> Double -> Picture
bmp w h (xc :+ yc) picScale = bitmapOfByteString w h bitmapFormat (bmpByteString w h xc yc picScale) False

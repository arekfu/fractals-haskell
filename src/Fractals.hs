module Fractals
    ( order
    , mandelbrot
    , bmp
    , toGrayscale
    , tangentPoint
    , mandelbrotCardioid
    ) where

import Data.Complex (Complex(..), magnitude, mkPolar)
import Graphics.Gloss
import qualified Data.ByteString as B
import Data.Word

-- | compute the order of a given complex function
order :: RealFloat a
      => (Complex a -> Complex a)   -- ^ a function on the complex plane
      -> a                          -- ^ the threshold
      -> Int                        -- ^ the maximum order
      -> Complex a                  -- ^ a starting point
      -> Int                        -- ^ the order
order f thresh nMax z0 = let f' = \ (i, z) -> (i+1, f z)
                             fs = dropWhile (\ (i, z) -> i<nMax && magnitude z < thresh) $ iterate f' (0, z0)
                          in fst $ head fs

-- | the Mandelbrot function: z^2 + c
mandelbrot :: RealFloat a
           => Complex a -- ^ the c parameter
           -> Complex a -- ^ the z variable
           -> Complex a -- ^ the resulting value
mandelbrot c z = z**2 + c

tangentPoint :: RealFloat a => Rational -> Complex a
tangentPoint r = mandelbrotCardioid mu
    where mu = mkPolar 1.0 (2.0 * pi * fromRational r)

mandelbrotCardioid :: RealFloat a => Complex a -> Complex a
mandelbrotCardioid mu = 0.5 * mu * (1.0 - 0.5 * mu)

bitmapFormat :: BitmapFormat
bitmapFormat = BitmapFormat BottomToTop PxRGBA

bmpValues :: Int -> Int -> Float -> Float -> Float -> [Word8]
bmpValues w h xc yc picScale = [ fromIntegral $ order (mandelbrot z0) 2.0 255 z0 |
    j <- [0..h-1], i <- [0..w-1],
    let x0 = xc - 0.5*wf + (fromIntegral i)*wf/(fromIntegral (w-1)),
    let y0 = yc - 0.5*hf + (fromIntegral j)*hf/(fromIntegral (h-1)),
    let z0 = x0 :+ y0
    ]
    where wf = fromIntegral w * picScale
          hf = fromIntegral h * picScale

toGrayscale :: [Word8] -> [Word8]
toGrayscale [] = []
toGrayscale (x:xs) = x:x:x:255:(toGrayscale xs)

bmpByteString :: Int -> Int -> Float -> Float -> Float -> B.ByteString
bmpByteString w h xc yc picScale = B.pack vals
    where vals = toGrayscale $ bmpValues w h xc yc picScale

bmp :: Int -> Int -> Float -> Float -> Float -> Picture
bmp w h xc yc picScale = bitmapOfByteString w h bitmapFormat (bmpByteString w h xc yc picScale) False

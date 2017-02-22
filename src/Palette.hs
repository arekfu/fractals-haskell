module Palette
( Palette
, grayscale
, hsv
) where

import Data.Word

type Palette = Double -> (Word8, Word8, Word8)

normToWord8 :: Double -> Word8
normToWord8 x = floor (255 * x)

grayscale :: Palette
grayscale x = let g = normToWord8 x in (g, g, g)

hsv :: Palette
hsv hue = let (hp, x) = properFraction $ 6.0 * hue :: (Int, Double)
              y = normToWord8 x
           in case hp of
                0 -> (255, y, 0)
                1 -> (255-y, 255, 0)
                2 -> (0, 255, y)
                3 -> (0, 255-y, 255)
                4 -> (y, 0, 255)
                _ -> (255, 0, 255-y)

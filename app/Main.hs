module Main where

--import Data.Complex
import Graphics.Gloss
import Data.Complex
import Data.Ratio
import Fractals

window :: Display
window = InWindow "Mandelbrot set" (1000, 600) (10, 10)

background :: Color
background = black

drawing :: Complex Double -> Picture
--drawing = bmp 1000 600 (((-0.1011):+0.9563)::Complex Double) 0.0000001
drawing z0 = bmp 1000 600 z0 0.0001

main :: IO ()
--main = print $ order (mandelbrot ((-1.6) :+ 0.0)) (2.0::Double) 256 (0.0 :+ 0.0)
main = print point >> (display window background $ drawing point)
    where point = (tangentPoint (5 % 11))::Complex Double

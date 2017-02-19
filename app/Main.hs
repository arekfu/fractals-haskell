module Main where

--import Data.Complex
import Graphics.Gloss
import Fractals

window :: Display
window = InWindow "Mandelbrot set" (1000, 600) (10, 10)

background :: Color
background = black

drawing :: Picture
drawing = bmp 1000 600 (-0.1011) 0.9563 0.0000001

main :: IO ()
--main = print $ order (mandelbrot ((-1.6) :+ 0.0)) (2.0::Double) 256 (0.0 :+ 0.0)
main = display window background drawing

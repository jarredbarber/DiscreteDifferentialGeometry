module Main where

import Lib
import qualified SComplex as SC

square = SC.fromList [[1,2,3], [2,3,4]]
faces = SC.faces square 2

main :: IO ()
main = putStrLn $ show faces

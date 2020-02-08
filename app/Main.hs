module Main where

import Lib
import qualified SComplex as SC

square = SC.fromList [[1,2,3], [2,3,4]]
-- >>= :: (a -> [b]) [a] [b]
faces = (SC.faces square 3) >>= SC.boundary

main :: IO ()
main = putStrLn $ show faces

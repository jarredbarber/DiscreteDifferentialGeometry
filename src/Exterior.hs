-- Exterior algebra on a simplical complex
module Exterior where

import qualified Data.Map.Strict as Map

import SComplex

data DiffForm a b = DiffForm { omega :: Simplex a -> b }

zero :: (Num b) => DiffForm a b
zero = DiffForm { omega = \s -> (fromInteger 0) }

wedge :: (Num b) => DiffForm a b -> DiffForm a b -> DiffForm a b
wedge u v = zero -- TBD

d :: (Num b) => DiffForm a b -> DiffForm a b
d df = DiffForm { omega = \s -> sum . map (omega df) $ boundary s }

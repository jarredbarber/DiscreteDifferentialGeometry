{-# LANGUAGE TypeSynonymInstances #-}
module SComplex where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- An n-simplex is defined by an ordered list of (n+1) vertices
type Simplex a = [a]

class BoundaryOperator a where
    boundary :: a -> [a]

null :: Simplex a
null = []

-- sends, e.g., [1,2,3,4] to [[1,2,3,4],[2,3,4],[3,4],[4],[]]
rep :: [a] -> [[a]]
rep [] = []
rep (x:xs) = [x:xs] ++ (rep xs)

-- The boundary operator maps a simplex to its boundary, which is a list of simplices of one lower order
instance BoundaryOperator (Simplex a) where
    -- boundary :: Simplex a -> [Simplex a]
    boundary [] = []
    boundary (_:[]) = []
    boundary vlist = map (take (l - 1)) $ take l $ rep $ cycle vlist
        where l = length vlist

-- boundary, boundary^2, etc
iterateBoundary :: Simplex a -> [Simplex a]
iterateBoundary [] = []
iterateBoundary s =
  let b = boundary s
      in
    b ++ (>>=) b iterateBoundary

-- A simplical complex is a collection of simplices, plus all of their boundaries
data SimplicalComplex a = SimplicalComplex { parents::Map.Map (Simplex a) (Set.Set (Simplex a)) }
  deriving (Show)

empty = SimplicalComplex {parents=Map.empty}

insert :: (Ord a) => Simplex a -> SimplicalComplex a -> SimplicalComplex a
insert [] sc = sc
insert s sc =
  let b = boundary s
      sc' = foldl (\x y -> insert y x) sc b
      inserter m k =
        Map.insertWith Set.union k (Set.singleton s) m
  in
    -- sc' has all of the children of s inserted properly.
    -- Now, let's link up the direct children with s
    SimplicalComplex { parents=Map.insert s Set.empty $ foldl inserter (parents sc') b}

fromList :: (Ord a) => [Simplex a] -> SimplicalComplex a
fromList = foldl (\x y -> insert y x) $ empty


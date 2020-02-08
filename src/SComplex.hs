{-# LANGUAGE TypeSynonymInstances #-}
module SComplex (Simplex, SComplex, boundary, empty, insert, fromList, union, faces, star, orientable, order)
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- An n-simplex is defined by an ordered list of (n+1) vertices
type Simplex a = [a]

class BoundaryOperator a where
  boundary :: a -> [a]

-- The boundary operator maps a simplex to its boundary, which is a list of simplices of one lower order
instance BoundaryOperator (Simplex a) where
    -- boundary :: Simplex a -> [Simplex a]
    boundary [] = []
    boundary (_:[]) = []
    boundary vlist =  let l = length vlist
                          -- maps [1,2,3] to [[1,2,3],[2,3],[3]]
                          rep [] = []
                          rep (x:xs) = [x:xs] ++ (rep xs)
                      in
                        map (take (l - 1)) $ take l $ rep $ cycle vlist

-- boundary, boundary^2, etc
iterateBoundary :: Simplex a -> [Simplex a]
iterateBoundary [] = []
iterateBoundary s =
  let b = boundary s
      in
    b ++ (>>=) b iterateBoundary

-- A simplical complex is a collection of simplices, plus all of their boundaries
data SComplex a = SComplex { order::Int, orientable::Bool, parents::Map.Map (Simplex a) (Set.Set (Simplex a)) }
  deriving (Show)

empty = SComplex { order=0, orientable=True, parents=Map.empty }

insert :: (Ord a) => Simplex a -> SComplex a -> SComplex a
insert [] sc = sc
insert s sc =
  let b = boundary s
      sc' = foldl (flip insert) sc b
      inserter m k =
        Map.insertWith Set.union k (Set.singleton s) m
  in
    -- sc' has all of the children of s inserted properly.
    -- Now, let's link up the direct children with s
    SComplex { order=max ((length s) - 1) (order sc')
             , orientable=(orientable sc') && (not $ Map.member (reverse s) (parents sc))
             , parents=Map.insert s Set.empty $ foldl inserter (parents sc') b }

fromList :: (Ord a, Foldable t) => t (Simplex a) -> SComplex a
fromList = foldl (flip insert) empty

union :: (Ord a) => SComplex a -> SComplex a -> SComplex a
union sc1 sc2 =
  foldl (flip insert) sc1 (Map.keys $ parents sc2)

faces :: SComplex a -> Int -> [Simplex a]
faces sc o = filter ((==) o . length) $ Map.keys . parents $ sc

-- pre-image of the boundary operator
star :: (Ord a) => SComplex a -> Simplex a -> Maybe [Simplex a]
star sc s = fmap Set.toList $ Map.lookup s $ parents sc


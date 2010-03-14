{-# OPTIONS -O2 -Wall #-}
module Data.MapManyToOne
    (MapManyToOne,
     insert, delete,
     empty, singleton,
     lookup, member, (!),
     keysFor, uniqueKeyFor,
     makeBackwardMap,
     fromList, toList,
     keys, backKeys)
where

import Prelude hiding (lookup)
import Control.Exception
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

data (Ord k1, Ord k2) => MapManyToOne k1 k2 = MapManyToOne { forward :: Map k1 k2,
                                                             backward :: Map k2 (Set k1) }

instance (Ord k1, Ord k2, Show k1, Show k2) => Show (MapManyToOne k1 k2) where
    show m = show $ forward m
instance (Ord k1, Ord k2) => Eq (MapManyToOne k1 k2) where
    a == b = (forward a) == (forward b)
instance (Ord k1, Ord k2) => Ord (MapManyToOne k1 k2) where
    a `compare` b = (forward a) `compare` (forward b)

insert :: (Ord k1, Ord k2) => k1 -> k2 -> MapManyToOne k1 k2 -> MapManyToOne k1 k2
insert k1 k2 m =
    MapManyToOne { forward = Map.insert k1 k2 (forward m),
                   backward = addToSetMap k1 k2 (backward m) }

uniqueGet :: (Ord k) => Set k -> k
uniqueGet set = assert (1 == Set.size set)
                (head $ Set.toList set)

addToSetMap :: (Ord k1, Ord k2) => k1 -> k2 -> Map k2 (Set k1) -> Map k2 (Set k1)
addToSetMap k1 k2 setMap = Map.alter addK1 k2 setMap
    where
      addK1 Nothing = Just (Set.singleton k1)
      addK1 (Just s) = Just (Set.insert k1 s)

delFromSetMap :: (Ord k1, Ord k2) => k1 -> k2 -> Map k2 (Set k1) -> Map k2 (Set k1)
delFromSetMap k1 k2 setMap = Map.alter delK1 k2 setMap
    where
      delK1 Nothing = error "Must contain the value"
      delK1 (Just s) = let newSet = (Set.delete k1 s)
                       in if Set.null newSet then Nothing
                          else Just newSet
                       
empty :: (Ord k1, Ord k2) => MapManyToOne k1 k2
empty = MapManyToOne { forward = Map.empty, backward = Map.empty }

singleton :: (Ord k1, Ord k2) => k1 -> k2 -> MapManyToOne k1 k2
singleton k1 k2 = MapManyToOne { forward = Map.singleton k1 k2,
                                 backward = Map.singleton k2 $ Set.singleton k1 }

(!) :: (Ord k1, Ord k2) => MapManyToOne k1 k2 -> k1 -> k2
m ! k1 = forward m Map.! k1

lookup :: (Ord k1, Ord k2) => MapManyToOne k1 k2 -> k1 -> Maybe k2
lookup m k1 = if member k1 m then Just (m!k1) else Nothing
           
keysFor :: (Ord k1, Ord k2) => k2 -> MapManyToOne k1 k2 -> Set k1
keysFor k2 m = backward m Map.! k2

uniqueKeyFor :: (Ord k1, Ord k2) => k2 -> MapManyToOne k1 k2 -> k1
uniqueKeyFor k2 m = uniqueGet $ keysFor k2 m
                 
member :: (Ord k1, Ord k2) => k1 -> MapManyToOne k1 k2 -> Bool
member k1 m = Map.member k1 (forward m)

makeBackwardMap :: (Ord k1, Ord k2) => [(k1, k2)] -> Map k2 (Set k1)
makeBackwardMap [] = Map.empty
makeBackwardMap ((k1, k2):xs) = addToSetMap k1 k2 (makeBackwardMap xs)

fromList :: (Ord k1, Ord k2) => [(k1, k2)] -> MapManyToOne k1 k2
fromList list = MapManyToOne { forward = (Map.fromList list),
                               backward = makeBackwardMap list }

toList :: (Ord k1, Ord k2) => MapManyToOne k1 k2 -> [(k1, k2)]
toList m = Map.toList (forward m)

keys :: (Ord k1, Ord k2) => MapManyToOne k1 k2 -> [k1]
keys m = Map.keys (forward m)

backKeys :: (Ord k1, Ord k2) => MapManyToOne k1 k2 -> [k2]
backKeys m = Map.keys (backward m)

delete :: (Ord k1, Ord k2) => k1 -> MapManyToOne k1 k2 -> MapManyToOne k1 k2
delete k1 m = MapManyToOne { forward = (Map.delete k1 (forward m)),
                             backward = delFromSetMap k1 (forward m Map.! k1) (backward m)}

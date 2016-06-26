-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Graph where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Prelude hiding (null)

data Ord v => Graph v = Graph {
  vertices :: Set v,

  -- Edge are of the form fst -> snd, meaning that fst depends on snd.
  edges    :: Set (v, v)
}

-- Returns all vertices in the graph which have an outgoing edge.
nonRoots :: Ord v => Graph v -> Set v
nonRoots graph = Set.map fst $ edges graph

-- Returns all vertices in the graph which have no outgoing edge.
roots :: Ord v => Graph v -> Set v
roots graph = (vertices graph) `Set.difference` (nonRoots graph)

-- Removes the given vertices from the graph.
remove :: Ord v => Set v -> Graph v -> Graph v
remove excl graph =
  let newVertices           = (vertices graph) `Set.difference` excl
      shouldDrop (from, to) = (from `Set.member` excl) || (to `Set.member` excl)
      newEdges              = Set.filter (not . shouldDrop) (edges graph)
  in  Graph { vertices = newVertices, edges = newEdges }

-- Removes root vertices from the graph.
removeRoots :: Ord v => Graph v -> Graph v
removeRoots graph = remove (roots graph) graph

-- Returns whether the graph is empty.
null :: Ord v => Graph v -> Bool
null graph = Set.null $ vertices graph

-- Returns a list of layers where every layer only has dependencies on layers
-- before it. The first layer contains all root vertices.
layers :: Ord v => Graph v -> [Set v]
layers graph = reverse $ aux [] graph
  where aux current gr | null graph          = current
        -- If the graph is not empty, but it has no roots, then it contains
        -- cycles. These are all put in the last layer. It would be possible to
        -- extract a little more structure by removing sets of leaves until
        -- there are no leaves left, but when we have cycles, all hope is lost
        -- anyway.
        aux current gr | Set.null $ roots gr = vertices gr : current
        aux current gr | otherwise           = aux (roots gr : current) (removeRoots gr)

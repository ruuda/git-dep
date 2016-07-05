-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE DeriveFunctor #-}

module GraphTree
(
  Graph,
  Tree,
  annotateDepth,
  buildForest,
  edges,
  flatten,
  indent,
  makeGraph,
  vertices
)
where

import           Data.List (find)
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Prelude hiding (null)

data Ord v => Graph v
  = Graph { vertices :: Set v
          , edges    :: Set (v, v) -- Edges are of the form fst -> snd,
          } deriving (Show)        -- meaning that fst depends on snd.

-- Given a set of vertices and a set of edges, constructs a valid graph. This
-- ensures that all vertices which are edge endpoints are also present in the
-- vertex collection.
makeGraph :: Ord v => Set v -> Set (v, v) -> Graph v
makeGraph vertices edges =
  let outgoing    = Set.map fst edges
      incoming    = Set.map snd edges
      allVertices = Set.unions [vertices, outgoing, incoming]
  in  Graph { vertices = allVertices, edges = edges }

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

-- Returns the direct dependencies of the given graph vertex, the vertices
-- reached by following the outgoing edges.
dependencies :: Ord v => Graph v -> v -> Set v
dependencies graph vtx = Set.map snd $ Set.filter ((== vtx) . fst) $ edges graph

-- Returns the vertices that depend directly on the given vertex, the vertices
-- reached by following the incoming edges.
dependants :: Ord v => Graph v -> v -> Set v
dependants graph vtx = Set.map fst $ Set.filter ((== vtx) . snd) $ edges graph

-- Returns a list of layers where every layer only has dependencies on layers
-- before it. The first layer contains all root vertices.
layers :: Ord v => Graph v -> [Set v]
layers graph = reverse $ aux [] graph
  where aux current gr | null gr             = current
        -- If the graph is not empty, but it has no roots, then it contains
        -- cycles. These are all put in the last layer. It would be possible to
        -- extract a little more structure by removing sets of leaves until
        -- there are no leaves left, but when we have cycles, all hope is lost
        -- anyway.
        aux current gr | Set.null $ roots gr = vertices gr : current
        aux current gr | otherwise           = aux (roots gr : current) (removeRoots gr)

-- A generic tree data structure.
data Tree a = Node a [Tree a] deriving (Functor, Show)

treeRoot :: Tree a -> a
treeRoot (Node root _) = root

treeChildren :: Tree a -> [Tree a]
treeChildren (Node _ children) = children

-- Turns a graph into a list of trees, where every child is a dependency of its
-- parent. If a vertex has multiple outgoing edges, then it will appear multiple
-- times in the tree.
-- TODO: Find a way to avoid duplicated subtrees.
buildForest :: Ord v => Graph v -> [Tree v]
buildForest graph =
  let rootList               = Set.toList $ roots graph
      rootDeps               = fmap (dependants graph) rootList
      childForest            = buildForest $ removeRoots graph
      findChildTree child    = find ((== child) . treeRoot) childForest
      getChildTree child     = fromMaybe (Node child []) $ findChildTree child
      buildTree (root, deps) = Node root $ fmap getChildTree $ Set.toList deps
  in  fmap buildTree $ rootList `zip` rootDeps

-- Assigns to every node in the tree its depth (starting from 0).
annotateDepth :: Tree v -> Tree (Int, v)
annotateDepth = aux 0
  where aux depth (Node v children) = Node (depth, v) $ fmap (aux (depth + 1)) children

-- Convert a tree into a tree of strings with two spaces indentation per level.
indent :: Show v => Tree v -> Tree String
indent = fmap print . annotateDepth
  where print (depth, value) = (spaces depth) ++ (show value)
        spaces n             = take (n * 2) $ repeat ' '

-- Converts the tree into a list by traversing it in pre-order.
flatten :: Tree v -> [v]
flatten (Node v children) = v : concatMap flatten children

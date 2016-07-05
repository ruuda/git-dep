-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Command (add, graph, rebase, remove, status) where

import           Control.Monad (forM_)
import qualified Data.Set as Set

import Git (getCurrentBranch, getDependencies, getTransitiveDependencies, listBranches)
import GitPlumbing (liftIO, runGit)
import GraphTree (buildForest, indent, makeGraph, flatten)

add :: [String] -> IO ()
add _args = undefined

remove :: [String] -> IO ()
remove _args = undefined

rebase :: [String] -> IO ()
rebase _args = undefined

status :: [String] -> IO ()
status _args = runGit $ do
  branches <- listBranches
  liftIO $ putStrLn "branches: "
  liftIO $ forM_ branches (putStrLn . show)
  liftIO $ putStrLn "deps: "
  forM_ branches $ \ branch -> do
    deps <- getDependencies branch
    forM_ deps $ \ dep -> do
      liftIO $ putStrLn $ (show branch) ++ " -> " ++ (show dep)

graph :: [String] -> IO ()
graph _args = runGit $ do
  branch <- getCurrentBranch
  deps   <- getTransitiveDependencies branch
  -- TODO: Extract into pure function?
  let depGraph = makeGraph (Set.singleton branch) deps
      depTrees = buildForest depGraph
      depList  = concatMap (flatten . indent) depTrees
  forM_ depList $ \ br -> do
    liftIO $ putStrLn br

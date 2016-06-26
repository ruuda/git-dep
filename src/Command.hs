-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Command (add, graph, rebase, remove, status) where

import Control.Monad (forM_)

import Git (getDependencies, listBranches)
import GitPlumbing (liftIO, runGit)

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
graph _args = undefined

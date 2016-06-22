-- Copyright 2016 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Command (add, graph, rebase, remove, status) where

import           Control.Monad (forM_)
import qualified Data.Text.IO as Text
import           Git

import RepositoryUtils (withCurrentRepository)

add :: [String] -> IO ()
add _args = do
  refs <- withCurrentRepository $ do
    listReferences
  forM_ refs Text.putStrLn
  putStrLn "not implemented: add"

remove :: [String] -> IO ()
remove _args = undefined

rebase :: [String] -> IO ()
rebase _args = undefined

status :: [String] -> IO ()
status _args = undefined

graph :: [String] -> IO ()
graph _args = undefined
